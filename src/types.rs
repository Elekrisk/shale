use std::{any::type_name, collections::HashMap};


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeID(usize);

pub enum Type {
    ConcreteStruct(ConcreteStructType)
}

impl Type {
    pub fn get_size(&self, types: &HashMap<TypeID, Type>) -> Result<usize, String> {
        match self {
            Type::ConcreteStruct(v) => v.get_size(types)
        }
    }
    pub fn get_align(&self, types: &HashMap<TypeID, Type>) -> Result<usize, String> {
        match self {
            Type::ConcreteStruct(v) => v.get_align(types)
        }
    }
    pub fn get_stride(&self, types: &HashMap<TypeID, Type>) -> Result<usize, String> {
        let size = self.get_size(types)?;
        let align = self.get_align(types)?;
        let stride = size + align - size % align;
        Ok(stride)
    }
}

pub struct ConcreteStructType {
    pub id: TypeID,
    pub fields: Vec<(String, TypeID)>
}

impl ConcreteStructType {
    pub fn get_size(&self, types: &HashMap<TypeID, Type>) -> Result<usize, String> {
        let mut size = 0;
        for (_, id) in &self.fields {
            let field_align = types[id].get_align(types)?;
            size = size + field_align - size % field_align;
            size += types[id].get_size(types)?;
        }

        Ok(size)
    }
    pub fn get_align(&self, types: &HashMap<TypeID, Type>) -> Result<usize, String> {
        let mut align = 1;
        for (_, id) in &self.fields {
            align = align.max(types[id].get_align(types)?);
        }

        Ok(align)
    }
}
