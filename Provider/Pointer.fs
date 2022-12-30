module JsonSchema.Pointer

let attribute (attr: string) =
    Json.Pointer.PointerSegment.op_Implicit attr

let index (index: int) =
    Json.Pointer.PointerSegment.op_Implicit (uint32 index)
