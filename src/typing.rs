use core::fmt;

// Variant polymorphique, version "les variants polymorphiques n'existent pas
// dans ce langage ET statiquement déterminés".
pub trait BasisTypable: Sized + PartialEq {
    const VOID: Self;
    const INT: Self;
    const BOOL: Self;

    fn is_eq(left: &Type<Self>, right: &Type<Self>) -> bool;
    fn to_basic(self) -> Option<BasisType> {
        if self == Self::BOOL {
            Some(BasisType::Bool)
        } else if self == Self::INT {
            Some(BasisType::Int)
        } else if self == Self::VOID {
            Some(BasisType::Void)
        } else {
            None
        }
    }
    // Rust does not allow this *yet*.
    // fn to_basic(self) -> Option<BasisType> {
    // 	match self {
    // 	    Self::BOOL => Some(BasisType::Bool),
    // 	    Self::INT => Some(BasisType::Bool),
    // 	    Self::VOID => Some(BasisType::Void),
    // 	    _ => None,
    // 	}
    // }
    fn from_basic(basic: BasisType) -> Self {
        match basic {
            BasisType::Bool => Self::BOOL,
            BasisType::Int => Self::INT,
            BasisType::Void => Self::VOID,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Type<B: BasisTypable = BasisType> {
    pub basis: B,
    pub indirection_count: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BasisType {
    Void,
    Int,
    Bool,
}

impl BasisTypable for BasisType {
    const VOID: Self = Self::Void;
    const INT: Self = Self::Int;
    const BOOL: Self = Self::Bool;

    fn is_eq(left: &Type<Self>, right: &Type<Self>) -> bool {
        match (left, right) {
            (
                Type {
                    basis: basis1,
                    indirection_count: cnt1,
                },
                Type {
                    basis: basis2,
                    indirection_count: cnt2,
                },
            ) if basis1 == basis2 && cnt1 == cnt2 => true,
            (
                Type {
                    basis: BasisType::Bool | BasisType::Int,
                    indirection_count: 0,
                },
                Type {
                    basis: BasisType::Bool | BasisType::Int,
                    indirection_count: 0,
                },
            ) => true,
            (
                Type {
                    indirection_count: cnt1,
                    ..
                },
                Type {
                    basis,
                    indirection_count: cnt2,
                },
            )
            | (
                Type {
                    basis,
                    indirection_count: cnt1,
                },
                Type {
                    indirection_count: cnt2,
                    ..
                },
            ) if *basis == BasisType::Void && *cnt1 > 0 && *cnt2 > 0 => true,
            _ => false,
        }
    }
}

impl fmt::Display for BasisType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Int => write!(f, "int"),
            Self::Void => write!(f, "void"),
        }
    }
}

impl<B: BasisTypable> Type<B> {
    pub const VOID: Self = Self {
        basis: B::VOID,
        indirection_count: 0,
    };
    pub const INT: Self = Self {
        basis: B::INT,
        indirection_count: 0,
    };
    pub const BOOL: Self = Self {
        basis: B::BOOL,
        indirection_count: 0,
    };
    pub const VOID_PTR: Self = Self {
        basis: B::VOID,
        indirection_count: 1,
    };

    pub fn is_eq(&self, other: &Self) -> bool {
        B::is_eq(self, other)
    }

    pub fn ptr(self) -> Self {
        Self {
            indirection_count: self.indirection_count + 1,
            ..self
        }
    }

    pub fn is_ptr(&self) -> bool {
        self.indirection_count > 0
    }

    pub fn to_basic(self) -> Option<Type> {
        Some(Type {
            basis: self.basis.to_basic()?,
            indirection_count: self.indirection_count,
        })
    }

    pub fn deref_ptr(self) -> Option<Self> {
        Some(Self {
            indirection_count: self.indirection_count.checked_sub(1)?,
            ..self
        })
    }
}

impl Type {
    pub fn from_basic<B: BasisTypable>(self) -> Type<B> {
        Type {
            basis: B::from_basic(self.basis),
            indirection_count: self.indirection_count,
        }
    }
}

impl<B: BasisTypable + fmt::Display> std::fmt::Display for Type<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.basis, "*".repeat(self.indirection_count))
    }
}
