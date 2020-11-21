/* This is dvipdfmx, an eXtended version of dvipdfm by Mark A. Wicks.

    Copyright (C) 2002-2016 by Jin-Hwan Cho and Shunsaku Hirata,
    the dvipdfmx project team.

    Copyright (C) 1998, 1999 by Mark A. Wicks <mwicks@kettering.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
*/

use arrayvec::ArrayString;
use core::iter::Peekable;
use core::str::Chars;

#[derive(Clone, Default, Debug)]
pub(crate) struct OtlOpt {
    rule: BtNode,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Operation {
    Or,
    And,
}

impl Default for Operation {
    fn default() -> Self {
        Operation::Or
    }
}

#[derive(Clone, Default, Debug)]
struct BtNode {
    operation: Operation,
    inverted: bool,
    left: Option<Box<BtNode>>,
    right: Option<Box<BtNode>>,
    data: ArrayString<[u8; 4]>,
}

impl OtlOpt {
    pub(crate) fn match_expr(&self, key: &str) -> bool {
        self.rule.match_expr(key)
    }

    pub(crate) fn parse_optstring(optstr: &str) -> Self {
        OtlOpt {
            rule: BtNode::parse_expr(&mut optstr.chars().peekable()).unwrap(),
        }
    }
}

impl BtNode {
    fn parse_expr(chars: &mut Peekable<Chars>) -> Option<Self> {
        let mut root = BtNode::default();
        let mut curr = &mut root;
        while let Some(&c) = chars.peek() {
            match c {
                '!' => {
                    chars.next();
                    if curr.operation == Operation::And {
                        curr.inverted = false;
                    } else {
                        curr.inverted = true;
                    }
                }
                '(' => {
                    chars.next();
                    let expr = if let Some(expr) = BtNode::parse_expr(chars) {
                        expr
                    } else {
                        warn!("Syntax error\n");
                        return None;
                    };
                    if Some(')') != chars.next() {
                        warn!("Syntax error: Unbalanced ()\n");
                        return None;
                    }
                    curr.left = expr.left;
                    curr.right = expr.right;
                    curr.data = expr.data;
                }
                ')' => return Some(root),
                '|' | '&' => {
                    chars.next();
                    let mut new_root = BtNode::default();
                    new_root.left = Some(Box::new(root));
                    new_root.right = Some(Box::new(BtNode::default()));
                    new_root.operation = if c == '&' {
                        Operation::And
                    } else {
                        Operation::Or
                    };
                    root = new_root;
                    curr = root.right.as_mut().unwrap();
                }
                '*' => {
                    chars.next();
                    curr.data = ArrayString::from("????").unwrap();
                }
                _ => {
                    for _ in 0..4 {
                        if let Some(c) = chars.next() {
                            match c {
                                ' ' | '?' => {
                                    curr.data.push(c);
                                }
                                a if a.is_ascii_alphanumeric() => {
                                    curr.data.push(a);
                                }
                                '_' => {
                                    curr.data.push(' ');
                                }
                                other => {
                                    warn!("Invalid char in tag: {}\n", other);
                                }
                            }
                        } else {
                            warn!("Syntax error\n");
                            return None;
                        }
                    }
                }
            }
        }
        Some(root)
    }

    fn match_expr(&self, key: &str) -> bool {
        self.match_expr_char_iter(&mut key.chars())
    }

    fn match_expr_char_iter(&self, key: &mut impl Iterator<Item = char>) -> bool {
        let mut retval = true;
        if self.left.is_none() && self.right.is_none() {
            retval = self.data.chars().zip(key).all(|(a, b)| a == '?' || a == b);
        } else {
            if let Some(left) = self.left.as_ref() {
                retval = left.match_expr_char_iter(key);
            }
            if let Some(right) = self.right.as_ref() {
                if retval && self.operation == Operation::And {
                    retval &= right.match_expr_char_iter(key);
                } else if !retval && self.operation == Operation::Or {
                    retval = right.match_expr_char_iter(key);
                }
            }
        }
        if self.inverted {
            retval = !retval;
        }
        retval
    }
}
