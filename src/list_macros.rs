use crate::Datum;




pub fn list_for_each<F>(list: Datum, mut f: F) 
    where F: for<'root> FnMut(Datum<'root>) 
{

}
