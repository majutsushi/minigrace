var kwyj1 := 1
var kwyj2 := 2
import util

// This module contains pseudo-classes for all the AST nodes used
// in the parser. The module predates the existence of classes in the
// implementation, so they are written as object literals inside methods.
// Each node has a different signature according to its function, but the
// common interface is:
// dtype ASTNode {
//   kind -> String // Used for pseudo-instanceof tests.
//   register -> String // Used later on to hold the LLVM register of
//                      // the resulting object.
//   line -> Number // The source line the node came from.
//   pretty(n:Number) -> String // Pretty-print of node at depth n,
// }
// Most also contain "value", with varied dtypes, holding the main value
// in the node. Some contain other fields for their specific use: while has
// both a value (the condition) and a "body", for example. None of the nodes
// are particularly notable in any way.

type ForNode = {
    forNodeID
    value
    body
}
class forNode.new(over, body') {
    def forNodeID = 1
    def kind = "for"
    def value = over
    def body = body'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitFor(self)) then {
            self.value.accept(visitor)
            self.body.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "For\n"
        s := s ++ spc ++ self.value.pretty(depth+1)
        s := s ++ "\n"
        s := s ++ spc ++ "Do:"
        s := s ++ "\n" ++ spc ++ "  " ++ self.body.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "for ({self.value.toGrace(0)}) do "
        s := s ++ self.body.toGrace(depth)
        s
    }
}
type WhileNode = {
    whileNodeID
    value
    body
}
class whileNode.new(cond, body') {
    def whileNodeID = 1
    def kind = "while"
    def value = cond
    def body = body'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitWhile(self)) then {
            self.value.accept(visitor)
            for (self.body) do { x ->
                x.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "While\n"
        s := s ++ spc ++ self.value.pretty(depth+1)
        s := s ++ "\n"
        s := s ++ spc ++ "Do:"
        for (self.body) do { x ->
            s := s ++ "\n  "++ spc ++ x.pretty(depth+2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "while \{{self.value.toGrace(depth + 1)}\} do \{"
        for (self.body) do { x ->
            s := s ++ "\n" ++ spc ++ "    " ++ x.toGrace(depth + 1)
        }
        s := s ++ "\n" ++ spc ++ "\}"
        s
    }
}
type IfNode = {
    ifNodeID
    value
    thenblock
    elseblock
}
class ifNode.new(cond, thenblock', elseblock') {
    def ifNodeID = 1
    def kind = "if"
    def value = cond
    def thenblock = thenblock'
    def elseblock = elseblock'
    var register := ""
    def line = util.linenum
    var handledIdentifiers := false
    method accept(visitor : ASTVisitor) {
        if (visitor.visitIf(self)) then {
            self.value.accept(visitor)
            for (self.thenblock) do { ix ->
                ix.accept(visitor)
            }
            for (self.elseblock) do { ix ->
                ix.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "If\n"
        s := s ++ spc ++ self.value.pretty(depth+1)
        s := s ++ "\n"
        s := s ++ spc ++ "Then:"
        for (self.thenblock) do { ix ->
            s := s ++ "\n  "++ spc ++ ix.pretty(depth+2)
        }
        s := s ++ "\n"
        s := s ++ spc ++ "Else:"
        for (self.elseblock) do { ix ->
            s := s ++ "\n  "++ spc ++ ix.pretty(depth+2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "if ({self.value.toGrace(0)}) then \{"
        for (self.thenblock) do { ix ->
            s := s ++ "\n" ++ spc ++ "    " ++ ix.toGrace(depth + 1)
        }
        if (self.elseblock.size > 0) then {
            s := s ++ "\n" ++ spc ++ "\} else \{"
            for (self.elseblock) do { ix ->
                s := s ++ "\n" ++ spc ++ "    " ++ ix.toGrace(depth + 1)
            }
        }
        s := s ++ "\n" ++ spc ++ "\}"
        s
    }
}
type BlockNode = {
    blockNodeID
    value
    params
    body
    matchingPattern
}
class blockNode.new(params', body') {
    def blockNodeID = 1
    def kind = "block"
    def value = "block"
    def params = params'
    def body = body'
    def selfclosure = true
    var register := ""
    var matchingPattern := false
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitBlock(self)) then {
            for (self.params) do { mx ->
                mx.accept(visitor)
            }
            for (self.body) do { mx ->
                mx.accept(visitor)
            }
            if (self.matchingPattern != false) then {
                self.matchingPattern.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Block\n"
        s := s ++ spc ++ "Parameters:"
        for (self.params) do { mx ->
            s := s ++ "\n  "++ spc ++ mx.pretty(depth+2)
        }
        s := s ++ "\n"
        s := s ++ spc ++ "Body:"
        for (self.body) do { mx ->
            s := s ++ "\n  "++ spc ++ mx.pretty(depth+2)
        }
        if (matchingPattern != false) then {
            s := s ++ "\n"
            s := s ++ spc ++ "Pattern:"
            s := s ++ "\n  "++ spc ++ matchingPattern.pretty(depth+2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "\{"
        if (self.params.size > 0) then {
            s := s ++ " "
            for (self.params.indices) do { i ->
                var p := self.params[i]
                if (matchingPattern != false) then {
                    s := s ++ "(" ++ p.toGrace(0) ++ ")"
                } else {
                    s := s ++ p.toGrace(0)
                }
                if (i < self.params.size) then {
                    s := s ++ ", "
                } else {
                    s := s ++ " ->"
                }
            }
        }
        for (self.body) do { mx ->
            s := s ++ "\n" ++ spc ++ "    " ++ mx.toGrace(depth + 1)
        }
        s := s ++ "\n" ++ spc ++ "\}"
        s
    }
}
type MatchCaseNode = {
    matchCaseID
    value
    cases
    elsecase
}
class matchCaseNode.new(matchee, cases', elsecase') {
    def matchCaseID = 1
    def kind = "matchcase"
    def value = matchee
    def cases = cases'
    def elsecase = elsecase'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitMatchCase(self)) then {
            self.value.accept(visitor)
            for (self.cases) do { mx ->
                mx.accept(visitor)
            }
            if (self.elsecase != false) then {
                self.elsecase.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Match\n"
        s := s ++ spc ++ matchee.pretty(depth + 2)
        for (self.cases) do { mx ->
            s := s ++ "\n{spc}Case:\n{spc}  {mx.pretty(depth+2)}"
        }
        if (false != elsecase) then {
            s := s ++ "\n{spc}Else:\n{spc}  {elsecase.pretty(depth+2)}"
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "match(" ++ self.value.toGrace(0) ++ ")"
        for (self.cases) do { case ->
            s := s ++ "\n" ++ spc ++ "    " ++ "case " ++ case.toGrace(depth + 2)
        }
        if (elsecase != false) then {
            s := s ++ "\n" ++ spc ++ "    " ++ "else " ++ elsecase.toGrace(depth + 2)
        }
        s
    }
}
type MethodTypeNode = {
    methodTypeID
    value
    signature
    rtype
}
class methodTypeNode.new(name', signature', rtype') {
    // [signature]
    //     object {
    //         name := ""
    //         params := []
    //         vararg := false/identifier
    //     }
    //     object {
    //         name := ""
    //         params := []
    //         vararg := false/identifier
    //     }
    //     ...
    //     object {
    //         ...
    //     }
    def methodTypeID = 1
    def kind = "methodtype"
    def value = name'
    def signature = signature'
    def rtype = rtype'
    def line = util.linenum
    var register := ""
    method accept(visitor : ASTVisitor) {
        if (visitor.visitMethodType(self)) then {
            if (self.rtype != false) then {
                self.rtype.accept(visitor)
            }
            for (self.signature) do { part ->
                for (part.params) do { p ->
                    p.accept(visitor)
                }
                if (part.vararg != false) then {
                    part.vararg.accept(visitor)
                }
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "MethodType\n"
        s := "{s}{spc}Name: {value}\n"
        if (rtype /= false) then {
            s := "{s}{spc}Returns:\n  {spc}{rtype.value}\n"
        }
        s := "{s}{spc}Signature:"
        for (signature) do { part ->
            s := "{s}\n  {spc}Part: {part.name}"
            s := "{s}\n    {spc}Parameters:"
            for (part.params) do { p ->
                s := "{s}\n      {spc}{p.pretty(depth + 4)}"
            }
            if (part.vararg != false) then {
                s := "{s}\n    {spc}Vararg: {part.vararg.pretty(depth + 3)}"
            }
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var s := ""
        for (self.signature) do { part ->
            s := s ++ part.name
            if ((part.params.size > 0) | (part.vararg != false)) then {
                s := s ++ "("
                for (part.params.indices) do { pnr ->
                    var p := part.params[pnr]
                    s := s ++ p.toGrace(depth + 1)
                    if ((pnr < part.params.size) | (part.vararg != false)) then {
                        s := s ++ ", "
                    }
                }
                if (part.vararg != false) then {
                    s := s ++ "*" ++ part.vararg.value
                }
                s := s ++ ")"
            }
        }
        if (self.rtype != false) then {
            s := s ++ " -> " ++ self.rtype.toGrace(depth + 1)
        }
        s
    }
}
type TypeNode = {
    typeNodeID
    value
    methods
    unionTypes
    intersectionTypes
    generics
}
class typeNode.new(name', methods') {
    def typeNodeID = 1
    def kind = "type"
    def value = name'
    def methods = methods'
    def unionTypes = []
    def intersectionTypes = []
    def line = util.linenum
    var generics := []
    var nominal := false
    var register := ""
    method accept(visitor : ASTVisitor) {
        if (visitor.visitType(self)) then {
            if (self.unionTypes.size > 0) then {
                for (self.unionTypes) do { ut ->
                    ut.accept(visitor)
                }
            }
            if (self.intersectionTypes.size > 0) then {
                for (self.intersectionTypes) do { it ->
                    it.accept(visitor)
                }
            }
            for (self.methods) do { mx ->
                mx.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Type\n"
        s := "{s}{spc}Name: {value}\n"
        if (unionTypes.size > 0) then {
            s := "{s}{spc}Union of:\n"
            for (unionTypes) do {ut->
                s := "{s}{spc}  {ut.value}\n"
            }
        }
        if (intersectionTypes.size > 0) then {
            s := "{s}{spc}Intersection of:\n"
            for (intersectionTypes) do {it->
                s := "{s}{spc}  {it.value}\n"
            }
        }
        s := s ++ spc ++ "Methods:"
        for (methods) do { mx ->
            s := s ++ "\n  "++ spc ++ mx.pretty(depth+2)
        }
        s := s ++ "\n"
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := ""
        def isanon = self.value.substringFrom(2)to(6) == "Anon_"
        def isadhoc = (self.value.substringFrom(1)to(6) == "Union<") |
                      (self.value.substringFrom(1)to(13) == "Intersection<")
        if (!isanon & !isadhoc) then {
            s := "type {self.value}"
            if (generics.size > 0) then {
                s := s ++ "<"
                for (generics.indices) do { i ->
                    s := s ++ generics[i].value
                    if (i < generics.size) then {
                        s := s ++ ", "
                    }
                }
                s := s ++ ">"
            }
            s := s ++ " = "
        }
        if (!isadhoc & (self.unionTypes.size == 0) &
            (self.intersectionTypes.size == 0)) then {
            s := s ++ "\{"
        }
        // TODO: what about e.g. "(A & B) | C"?
        if (self.unionTypes.size > 0) then {
            for (self.unionTypes.indices) do { i ->
                s := s ++ self.unionTypes[i].toGrace(0)
                if (i < self.unionTypes.size) then {
                    s := s ++ " | "
                }
            }
        } elseif (self.intersectionTypes.size > 0) then {
            for (self.intersectionTypes.indices) do { i ->
                s := s ++ self.intersectionTypes[i].toGrace(0)
                if (i < self.intersectionTypes.size) then {
                    s := s ++ " & "
                }
            }
        }
        for (self.methods) do { mx ->
            s := s ++ "\n" ++ spc ++ "    " ++ mx.toGrace(depth + 1)
        }
        if (!isadhoc & (self.unionTypes.size == 0) &
            (self.intersectionTypes.size == 0)) then {
            s := s ++ "\n" ++ spc ++ "\}"
        }
        s
    }
}
type MethodNode = {
    methodNodeID
    value
    signature
    body
    dtype
}
class methodNode.new(name', signature', body', dtype') {
    // [signature]
    //     object {
    //         name := ""
    //         params := []
    //         vararg := false/identifier
    //     }
    //     object {
    //         name := ""
    //         params := []
    //         vararg := false/identifier
    //     }
    //     ...
    //     object {
    //         ...
    //     }
    def methodNodeID = 1
    def kind = "method"
    def value = name'
    def signature = signature'
    def body = body'
    var dtype := dtype'
    var varargs := false
    var selfclosure := false
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitMethod(self)) then {
            self.value.accept(visitor)
            if (self.dtype != false) then {
                self.dtype.accept(visitor)
            }
            for (self.signature) do { part ->
                for (part.params) do { p ->
                    p.accept(visitor)
                }
                if (part.vararg != false) then {
                    part.vararg.accept(visitor)
                }
            }
            for (self.body) do { mx ->
                mx.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Method\n"
        s := s ++ spc ++ "Name: " ++ self.value.pretty(depth+1)
        s := s ++ "\n"
        s := "{s}{spc}Signature:"
        for (signature) do { part ->
            s := "{s}\n  {spc}Part: {part.name}"
            s := "{s}\n    {spc}Parameters:"
            for (part.params) do { p ->
                s := "{s}\n      {spc}{p.pretty(depth + 4)}"
            }
            if (part.vararg != false) then {
                s := "{s}\n    {spc}Vararg: {part.vararg.pretty(depth + 3)}"
            }
        }
        s := s ++ "\n"
        s := s ++ spc ++ "Body:"
        for (self.body) do { mx ->
            s := s ++ "\n  "++ spc ++ mx.pretty(depth+2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "method "
        for (self.signature) do { part ->
            s := s ++ part.name
            if ((part.params.size > 0) | (part.vararg != false)) then {
                s := s ++ "("
                for (part.params.indices) do { pnr ->
                    var p := part.params[pnr]
                    s := s ++ p.toGrace(depth + 1)
                    if ((pnr < part.params.size) | (part.vararg != false)) then {
                        s := s ++ ", "
                    }
                }
                if (part.vararg != false) then {
                    s := s ++ "*" ++ part.vararg.value
                }
                s := s ++ ")"
            }
        }
        if (self.dtype != false) then {
            s := s ++ " -> {self.dtype.toGrace(0)}"
        }
        s := s ++ " \{"
        for (self.body) do { mx ->
            s := s ++ "\n" ++ spc ++ "    " ++ mx.toGrace(depth + 1)
        }
        s := s ++ "\n" ++ spc ++ "\}"
        s
    }
}
type CallNode = {
    callNodeID
    value
    with
}
class callNode.new(what, with') {
    // [with]
    //     object {
    //         name := ""
    //         args := []
    //     }
    //     object {
    //         name := ""
    //         args := []
    //     }
    //     ...
    //     object {
    //         ...
    //     }
    def callNodeID = 1
    def kind = "call"
    def value = what
    def with = with'
    def line = 0 + util.linenum
    var register := ""
    method accept(visitor : ASTVisitor) {
        if (visitor.visitCall(self)) then {
            self.value.accept(visitor)
            for (self.with) do { part ->
                for (part.args) do { arg ->
                    arg.accept(visitor)
                }
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Call\n"
        s := s ++ spc ++ "Method: {self.value.pretty(depth + 1)}"
        s := s ++ "\n"
        s := s ++ spc ++ "Arguments:"
        for (self.with) do { part ->
            s := s ++ "\n  " ++ spc ++ "Part: " ++ part.name
            for (part.args) do { arg ->
                s := s ++ "\n    " ++ spc ++ arg.pretty(depth + 3)
            }
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := ""
        // only the last member is the method call we need to handle
        if (self.value.kind == "member") then {
            var member := self.value
            if (member.value.substringFrom(1)to(6) == "prefix") then {
                s := member.value.substringFrom(7)to(member.value.size)
                return s ++ member.in.toGrace(0)
            }
            s := member.in.toGrace(0) ++ "."
        }
        for (self.with) do { part ->
            s := s ++ part.name
            if (part.args.size > 0) then {
                s := s ++ "("
                for (part.args.indices) do { anr ->
                    var arg := part.args[anr]
                    s := s ++ arg.toGrace(depth + 1)
                    if (anr < part.args.size) then {
                        s := s ++ ", "
                    }
                }
                s := s ++ ")"
            }
        }
        s
    }
}
type ClassNode = {
    classNodeID
    value
    name
    signature
}
class classNode.new(name', signature', body', superclass', constructor') {
    // [signature]
    //     object {
    //         name := ""
    //         params := []
    //         vararg := false/identifier
    //     }
    //     object {
    //         name := ""
    //         params := []
    //         vararg := false/identifier
    //     }
    //     ...
    //     object {
    //         ...
    //     }
    def classNodeID = 1
    def kind = "class"
    def value = body'
    def name = name'
    def constructor = constructor'
    def signature = signature'
    var register := ""
    def line = util.linenum
    def superclass = superclass'
    method accept(visitor : ASTVisitor) {
        if (visitor.visitClass(self)) then {
            self.name.accept(visitor)
            self.constructor.accept(visitor)
            if (self.superclass != false) then {
                self.superclass.accept(visitor)
            }
            for (self.signature) do { part ->
                for (part.params) do { p ->
                    p.accept(visitor)
                }
                if (part.vararg != false) then {
                    part.vararg.accept(visitor)
                }
            }
            for (self.value) do { x ->
                x.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Class(" ++ self.name.pretty(0) ++ ")"
        if (self.superclass /= false) then {
            s := s ++ "\n" ++ spc ++ "Superclass:"
            s := s ++ "\n  " ++ spc ++ self.superclass.pretty(depth + 2)
        }
        s := s ++ "\n"
        s := "{s}{spc}Signature:"
        for (signature) do { part ->
            s := "{s}\n  {spc}Part: {part.name}"
            s := "{s}\n    {spc}Parameters:"
            for (part.params) do { p ->
                s := "{s}\n      {spc}{p.pretty(depth + 4)}"
            }
            if (part.vararg != false) then {
                s := "{s}\n    {spc}Vararg: {part.vararg.pretty(depth + 3)}"
            }
        }
        s := s ++ "\n" ++ spc ++ "Body:"
        for (self.value) do { x ->
            s := s ++ "\n  "++ spc ++ x.pretty(depth+2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "class {self.name.toGrace(0)}"
        if (self.name.kind != "generic") then {
            // TODO generics + new-style constructors aren't possible at the moment
            s := s ++ "."
            for (self.signature) do { part ->
                s := s ++ part.name
                if ((part.params.size > 0) | (part.vararg != false)) then {
                    s := s ++ "("
                    for (part.params.indices) do { pnr ->
                        var p := part.params[pnr]
                        s := s ++ p.toGrace(depth + 1)
                        if ((pnr < part.params.size) | (part.vararg != false)) then {
                            s := s ++ ", "
                        }
                    }
                    if (part.vararg != false) then {
                        s := s ++ "*" ++ part.vararg.value
                    }
                    s := s ++ ")"
                }
            }
        }
        s := s ++ " \{"
        for (self.value) do { mx ->
            s := s ++ "\n" ++ spc ++ "    " ++ mx.toGrace(depth + 1)
        }
        s := s ++ "\n" ++ spc ++ "\}"
        s
    }
}
type ObjectNode = {
    objectNodeID
    value
}
class objectNode.new(body, superclass') {
    def objectNodeID = 1
    def kind = "object"
    def value = body
    var register := ""
    def line = util.linenum
    def superclass = superclass'
    var otype := false
    var classname := "object"
    method accept(visitor : ASTVisitor) {
        if (visitor.visitObject(self)) then {
            if (self.superclass != false) then {
                self.superclass.accept(visitor)
            }
            for (self.value) do { x ->
                x.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Object"
        if (self.superclass /= false) then {
            s := s ++ "\n" ++ spc ++ "Superclass:"
            s := s ++ "\n  " ++ spc ++ self.superclass.pretty(depth + 1)
            s := s ++ "\n" ++ spc ++ "Body:"
            depth := depth + 1
        }
        for (self.value) do { x ->
            s := s ++ "\n"++ spc ++ x.pretty(depth+1)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "object \{"
        for (self.value) do { x ->
            s := s ++ "\n" ++ spc ++ "    " ++ x.toGrace(depth + 1)
        }
        s := s ++ "\n" ++ spc ++ "\}"
        s
    }
}
type ArrayNode = {
    arrayNodeID
    value
}
class arrayNode.new(values) {
    def arrayNodeID = 1
    def kind = "array"
    def value = values
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitArray(self)) then {
            for (self.value) do { ax ->
                ax.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { ai ->
            spc := spc ++ "  "
        }
        var s := "Array"
        for (self.value) do { ax ->
            s := s ++ "\n"++ spc ++ ax.pretty(depth+1)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var s := "["
        for (self.value.indices) do { i ->
            s := s ++ self.value[i].toGrace(0)
            if (i < self.value.size) then {
                s := s ++ ", "
            }
        }
        s := s ++ "]"
        s
    }
}
type MemberNode = {
    memberNodeID
    value
    in
}
class memberNode.new(what, in') {
    def memberNodeID = 1
    def kind = "member"
    var value := what
    def in = in'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitMember(self)) then {
            self.in.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Member(" ++ self.value ++ ")\n"
        s := s ++ spc ++ self.in.pretty(depth+1)
        s
    }
    method toGrace(depth : Number) -> String {
        var s := ""
        if (self.value.substringFrom(1)to(6) == "prefix") then {
            s := self.value.substringFrom(7)to(self.value.size)
            s := s ++ " " ++ self.in.toGrace(0)
        } else {
            s := self.in.toGrace(depth) ++ "." ++ self.value
        }
        s
    }
}
type GenericNode = {
    genericNodeID
    value
    params
}
class genericNode.new(base, params') {
    def genericNodeID = 1
    def kind = "generic"
    def value = base
    def params = params'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitGeneric(self)) then {
            self.value.accept(visitor)
            for (self.params) do { p ->
                p.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var s := "Generic(" ++ self.value.value ++ "<"
        for (params) do {p->
            s := s ++ p.pretty(0)
            s := s ++ ","
        }
        s ++ ">)"
    }
    method toGrace(depth : Number) -> String {
        var s := self.value.value ++ "<"
        for (self.params.indices) do { i ->
            s := s ++ self.params[i].toGrace(0)
            if (i < self.params.size) then {
                s := s ++ ", "
            }
        }
        s := s ++ ">"
        s
    }
}
type IdentifierNode = {
    identifierNodeID
    value
    dtype
}
class identifierNode.new(n, dtype') {
    def identifierNodeID = 1
    def kind = "identifier"
    var value := n
    var dtype := dtype'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitIdentifier(self)) then {
            if (self.dtype != false) then {
                self.dtype.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Identifier(" ++ self.value ++ ")"
        if (self.dtype != false) then {
            s := s ++ "\n" ++ spc ++ "Type:"
            s := s ++ "\n" ++ spc ++ "  " ++ self.dtype.pretty(depth + 2)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var s := self.value
        if (self.dtype != false) then {
            s := s ++ " : " ++ self.dtype.toGrace(depth + 1)
        }
        s
    }
}
type OctetsNode = {
    octetsNodeID
    value
}
class octetsNode.new(n) {
    def octetsNodeID = 1
    def kind = "octets"
    def value = n
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        visitor.visitOctets(self)
    }
    method pretty(depth) {
        "Octets(" ++ self.value ++ ")"
    }
    method toGrace(depth : Number) -> String {
        self.value
    }
}
type StringNode = {
    stringNodeID
    value
}
class stringNode.new(n) {
    def stringNodeID = 1
    def kind = "string"
    var value := n
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        visitor.visitString(self)
    }
    method pretty(depth) {
        "String(" ++ self.value ++ ")"
    }
    method toGrace(depth : Number) -> String {
        var s := "\""
        for (self.value) do { c ->
            // TODO: what escapes are missing?
            if (c == "\n") then {
                s := s ++ "\\n"
            } elseif (c == "\t") then {
                s := s ++ "\\t"
            } elseif (c == "\"") then {
                s := s ++ "\\\""
            } elseif (c == "\\") then {
                s := s ++ "\\\\"
            } else {
                s := s ++ c
            }
        }
        s := s ++ "\""
        s
    }
}
type NumNode = {
    numNodeID
    value
}
class numNode.new(n) {
    def numNodeID = 1
    def kind = "num"
    def value = n
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        visitor.visitNum(self)
    }
    method pretty(depth) {
        "Num(" ++ self.value ++ ")"
    }
    method toGrace(depth : Number) -> String {
        self.value.asString
    }
}
type OpNode = {
    opNodeID
    value
    left
    right
}
class opNode.new(op, l, r) {
    def opNodeID = 1
    def kind = "op"
    def value = op
    def left = l
    def right = r
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitOp(self)) then {
            self.left.accept(visitor)
            self.right.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Op(" ++ self.value ++ ")"
        s := s ++ "\n"
        s := s ++ spc ++ self.left.pretty(depth + 1)
        s := s ++ "\n"
        s := s ++ spc ++ self.right.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        var s := ""
        if ((self.left.kind == "op") & (self.left.value != self.value)) then {
            s := "(" ++ self.left.toGrace(0) ++ ")"
        } else {
            s := self.left.toGrace(0)
        }
        if (self.value == "..") then {
            s := s ++ self.value
        } else {
            s := s ++ " " ++ self.value ++ " "
        }
        if ((self.right.kind == "op") & (self.right.value != self.value)) then {
            s := s ++ "(" ++ self.right.toGrace(0) ++ ")"
        } else {
            s := s ++ self.right.toGrace(0)
        }
        s
    }
}
type IndexNode = {
    indexNodeID
    value
    index
}
class indexNode.new(expr, index') {
    def indexNodeID = 1
    def kind = "index"
    def value = expr
    def index = index'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitIndex(self)) then {
            self.value.accept(visitor)
            self.index.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Index"
        s := s ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        s := s ++ "\n"
        s := s ++ spc ++ self.index.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := self.value.toGrace(depth + 1)
        s := s ++ "[" ++ self.index.toGrace(depth + 1) ++ "]"
        s
    }
}
type BindNode = {
    bindNodeID
    dest
    value
}
class bindNode.new(dest', val') {
    def bindNodeID = 1
    def kind = "bind"
    def dest = dest'
    def value = val'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitBind(self)) then {
            self.dest.accept(visitor)
            self.value.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Bind"
        s := s ++ "\n"
        s := s ++ spc ++ self.dest.pretty(depth + 1)
        s := s ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := self.dest.toGrace(depth + 1)
        s := s ++ " := " ++ self.value.toGrace(depth + 1)
        s
    }
}
type DefDecNode = {
    defDecNodeID
    name
    value
    dtype
}
class defDecNode.new(name', val, dtype') {
    def defDecNodeID = 1
    def kind = "defdec"
    def name = name'
    def value = val
    var dtype := dtype'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitDefDec(self)) then {
            self.name.accept(visitor)
            if (self.dtype != false) then {
                self.dtype.accept(visitor)
            }
            if (self.value != false) then {
                self.value.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "defdec"
        s := s ++ "\n"
        s := s ++ spc ++ self.name.pretty(depth)
        if (self.dtype != false) then {
            s := s ++ "\n" ++ spc ++ "Type:"
            s := s ++ "\n" ++ spc ++ "  " ++ self.dtype.pretty(depth + 2)
        }
        if (false != self.value) then {
            s := s ++ "\n"
            s := s ++ spc ++ self.value.pretty(depth + 1)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "def {self.name.toGrace(0)}"
        if (self.dtype.value != "Dynamic") then {
            s := s ++ " : " ++ self.dtype.toGrace(0)
        }
        if (self.value != false) then {
            s := s ++ " = " ++ self.value.toGrace(depth)
        }
        s
    }
}
type VarDecNode = {
    varDecNodeID
    name
    value
    dtype
}
class varDecNode.new(name', val', dtype') {
    def varDecNodeID = 1
    def kind = "vardec"
    def name = name'
    def value = val'
    var dtype := dtype'
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitVarDec(self)) then {
            self.name.accept(visitor)
            if (self.dtype != false) then {
                self.dtype.accept(visitor)
            }
            if (self.value != false) then {
                self.value.accept(visitor)
            }
        }
    }
    method pretty(depth) {
        var spc := ""
        for ((0..depth)) do { i ->
            spc := spc ++ "  "
        }
        var s := "VarDec"
        s := s ++ "\n"
        s := s ++ spc ++ self.name.pretty(depth + 1)
        if (self.dtype != false) then {
            s := s ++ "\n" ++ spc ++ "Type:"
            s := s ++ "\n" ++ spc ++ "  " ++ self.dtype.pretty(depth + 2)
        }
        if (false != self.value) then {
            s := s ++ "\n"
            s := s ++ spc ++ self.value.pretty(depth + 1)
        }
        s
    }
    method toGrace(depth : Number) -> String {
        var spc := ""
        for (0..(depth - 1)) do { i ->
            spc := spc ++ "    "
        }
        var s := "var {self.name.toGrace(0)}"
        if (self.dtype.value != "Dynamic") then {
            s := s ++ " : " ++ self.dtype.toGrace(0)
        }
        if (self.value != false) then {
            s := s ++ " := " ++ self.value.toGrace(depth)
        }
        s
    }
}
type ImportNode = {
    importNodeID
    value
}
class importNode.new(name) {
    def importNodeID = 1
    def kind = "import"
    def value = name
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitImport(self)) then {
            self.value.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Import"
        s := s ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        var s := "import " ++ self.value.toGrace(depth + 1)
        s
    }
}
type ReturnNode = {
    returnNodeID
    value
}
class returnNode.new(expr) {
    def returnNodeID = 1
    def kind = "return"
    def value = expr
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitReturn(self)) then {
            self.value.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Return"
        s := s ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        "return " ++ self.value.toGrace(depth)
    }
}
type InheritsNode = {
    inheritsNodeID
    value
}
class inheritsNode.new(expr) {
    def inheritsNodeID = 1
    def kind = "inherits"
    def value = expr
    var register := ""
    def line = util.linenum
    method accept(visitor : ASTVisitor) {
        if (visitor.visitInherits(self)) then {
            self.value.accept(visitor)
        }
    }
    method pretty(depth) {
        var spc := ""
        for (0..depth) do { i ->
            spc := spc ++ "  "
        }
        var s := "Inherits"
        s := s ++ "\n"
        s := s ++ spc ++ self.value.pretty(depth + 1)
        s
    }
    method toGrace(depth : Number) -> String {
        "inherits {self.value.toGrace(0)}"
    }
}

method signaturePart {
    object {
        method new(*values) {
            object {
                var name := ""
                var params := []
                var vararg := false
                if (values.size > 0) then {
                    name := values[1]
                }
                if (values.size > 1) then {
                    params := values[2]
                }
                if (values.size > 2) then {
                    vararg := values[3]
                }
            }
        }
    }
}

// class signaturePart.new(*values) {
//     var name := ""
//     var params := []
//     var vararg := false
//     if (values.size > 0) then {
//         name := values[1]
//     }
//     if (values.size > 1) then {
//         params := values[2]
//     }
//     if (values.size > 2) then {
//         vararg := values[3]
//     }
// }

method callWithPart {
    object {
        method new(*values) {
            object {
                var name := ""
                var args := []
                if (values.size > 0) then {
                    name := values[1]
                }
                if (values.size > 1) then {
                    args := values[2]
                }
            }
        }
    }
}

type ASTVisitor = {
     visitFor -> Boolean
     visitWhile -> Boolean
     visitIf -> Boolean
     visitBlock -> Boolean
     visitMatchCase -> Boolean
     visitMethodType -> Boolean
     visitType -> Boolean
     visitMethod -> Boolean
     visitCall -> Boolean
     visitClass -> Boolean
     visitObject -> Boolean
     visitArray -> Boolean
     visitMember -> Boolean
     visitGeneric -> Boolean
     visitIdentifier -> Boolean
     visitOctets -> Boolean
     visitString -> Boolean
     visitNum -> Boolean
     visitOp -> Boolean
     visitIndex -> Boolean
     visitBind -> Boolean
     visitDefDec -> Boolean
     visitVarDec -> Boolean
     visitImport -> Boolean
     visitReturn -> Boolean
     visitInherits -> Boolean
}

method toGraceMatch(val, depth) {
    match(val)
        case { node : StringNode ->
                    var s := "\""
                    for (node.value) do { c ->
                        if (c == "\n") then {
                            s := s ++ "\\n"
                        } elseif (c == "\t") then {
                            s := s ++ "\\t"
                        } elseif (c == "\"") then {
                            s := s ++ "\\\""
                        } elseif (c == "\\") then {
                            s := s ++ "\\\\"
                        } else {
                            s := s ++ c
                        }
                    }
                    s := s ++ "\""
                    return s
        }
        case { node : OpNode ->
                    var s := ""
                    if ((node.left.kind == "op") & (node.left.value != node.value)) then {
                        s := "(" ++ toGraceMatch(node.left, 0) ++ ")"
                    } else {
                        s := toGraceMatch(node.left, 0)
                    }
                    if (node.value == "..") then {
                        s := s ++ node.value
                    } else {
                        s := s ++ " " ++ node.value ++ " "
                    }
                    if ((node.right.kind == "op") & (node.right.value != node.value)) then {
                        s := s ++ "(" ++ toGraceMatch(node.right, 0) ++ ")"
                    } else {
                        s := s ++ toGraceMatch(node.right, 0)
                    }
                    return s
        }
        case { node : IdentifierNode ->
                    var s := node.value
                    if (node.dtype != false) then {
                        s := s ++ " : " ++ toGraceMatch(node.dtype, depth + 1)
                    }
                    return s
        }
        case { node : BindNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := toGraceMatch(node.dest, depth + 1)
                    s := s ++ " := " ++ toGraceMatch(node.value, depth + 1)
                    return s
        }
        case { node : NumNode ->
                    return node.value.asString
        }
        case { node : CallNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := ""
                    // only the last member is the method call we need to handle
                    if (node.value.kind == "member") then {
                        var member := node.value
                        if (member.value.substringFrom(1)to(6) == "prefix") then {
                            s := member.value.substringFrom(7)to(member.value.size)
                            return s ++ toGraceMatch(member.in, 0)
                        }
                        s := toGraceMatch(member.in, 0) ++ "."
                    }
                    for (node.with) do { part ->
                        s := s ++ part.name
                        if (part.args.size > 0) then {
                            s := s ++ "("
                            for (part.args.indices) do { anr ->
                                var arg := part.args[anr]
                                s := s ++ toGraceMatch(arg, depth + 1)
                                if (anr < part.args.size) then {
                                    s := s ++ ", "
                                }
                            }
                            s := s ++ ")"
                        }
                    }
                    return s
        }
        case { node : MemberNode ->
                    var s := ""
                    if (node.value.substringFrom(1)to(6) == "prefix") then {
                        s := node.value.substringFrom(7)to(node.value.size)
                        s := s ++ " " ++ toGraceMatch(node.in, 0)
                    } else {
                        s := toGraceMatch(node.in, depth) ++ "." ++ node.value
                    }
                    return s
        }
        case { node : IfNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "if ({toGraceMatch(node.value, 0)}) then \{"
                    for (node.thenblock) do { ix ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(ix, depth + 1)
                    }
                    if (node.elseblock.size > 0) then {
                        s := s ++ "\n" ++ spc ++ "\} else \{"
                        for (node.elseblock) do { ix ->
                            s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(ix, depth + 1)
                        }
                    }
                    s := s ++ "\n" ++ spc ++ "\}"
                    return s
        }
        case { node : BlockNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "\{"
                    if (node.params.size > 0) then {
                        s := s ++ " "
                        for (node.params.indices) do { i ->
                            var p := node.params[i]
                            if (node.matchingPattern != false) then {
                                s := s ++ "(" ++ toGraceMatch(p, 0) ++ ")"
                            } else {
                                s := s ++ toGraceMatch(p, 0)
                            }
                            if (i < node.params.size) then {
                                s := s ++ ", "
                            } else {
                                s := s ++ " ->"
                            }
                        }
                    }
                    for (node.body) do { mx ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(mx, depth + 1)
                    }
                    s := s ++ "\n" ++ spc ++ "\}"
                    return s
        }
        case { node : IndexNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := toGraceMatch(node.value, depth + 1)
                    s := s ++ "[" ++ toGraceMatch(node.index, depth + 1) ++ "]"
                    return s
        }
        case { node : MatchCaseNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "match(" ++ toGraceMatch(node.value, 0) ++ ")"
                    for (node.cases) do { case ->
                        s := s ++ "\n" ++ spc ++ "    " ++ "case " ++ toGraceMatch(case, depth + 2)
                    }
                    if (node.elsecase != false) then {
                        s := s ++ "\n" ++ spc ++ "    " ++ "else " ++ toGraceMatch(node.elsecase, depth + 2)
                    }
                    return s
        }
        case { node : MethodTypeNode ->
                    var s := ""
                    for (node.signature) do { part ->
                        s := s ++ part.name
                        if ((part.params.size > 0) | (part.vararg != false)) then {
                            s := s ++ "("
                            for (part.params.indices) do { pnr ->
                                var p := part.params[pnr]
                                s := s ++ toGraceMatch(p, depth + 1)
                                if ((pnr < part.params.size) | (part.vararg != false)) then {
                                    s := s ++ ", "
                                }
                            }
                            if (part.vararg != false) then {
                                s := s ++ "*" ++ part.vararg.value
                            }
                            s := s ++ ")"
                        }
                    }
                    if (node.rtype != false) then {
                        s := s ++ " -> " ++ toGraceMatch(node.rtype, depth + 1)
                    }
                    return s
        }
        case { node : TypeNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := ""
                    def isanon = node.value.substringFrom(2)to(6) == "Anon_"
                    def isadhoc = (node.value.substringFrom(1)to(6) == "Union<") |
                                  (node.value.substringFrom(1)to(13) == "Intersection<")
                    if (!isanon & !isadhoc) then {
                        s := "type {node.value}"
                        if (node.generics.size > 0) then {
                            s := s ++ "<"
                            for (node.generics.indices) do { i ->
                                s := s ++ node.generics[i].value
                                if (i < node.generics.size) then {
                                    s := s ++ ", "
                                }
                            }
                            s := s ++ ">"
                        }
                        s := s ++ " = "
                    }
                    if (!isadhoc & (node.unionTypes.size == 0) &
                        (node.intersectionTypes.size == 0)) then {
                        s := s ++ "\{"
                    }
                    // TODO: what about e.g. "(A & B) | C"?
                    if (node.unionTypes.size > 0) then {
                        for (node.unionTypes.indices) do { i ->
                            s := s ++ toGraceMatch(node.unionTypes[i], 0)
                            if (i < node.unionTypes.size) then {
                                s := s ++ " | "
                            }
                        }
                    } elseif (node.intersectionTypes.size > 0) then {
                        for (node.intersectionTypes.indices) do { i ->
                            s := s ++ toGraceMatch(node.intersectionTypes[i], 0)
                            if (i < node.intersectionTypes.size) then {
                                s := s ++ " & "
                            }
                        }
                    }
                    for (node.methods) do { mx ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(mx, depth + 1)
                    }
                    if (!isadhoc & (node.unionTypes.size == 0) &
                        (node.intersectionTypes.size == 0)) then {
                        s := s ++ "\n" ++ spc ++ "\}"
                    }
                    return s
        }
        case { node : MethodNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "method "
                    for (node.signature) do { part ->
                        s := s ++ part.name
                        if ((part.params.size > 0) | (part.vararg != false)) then {
                            s := s ++ "("
                            for (part.params.indices) do { pnr ->
                                var p := part.params[pnr]
                                s := s ++ toGraceMatch(p, depth + 1)
                                if ((pnr < part.params.size) | (part.vararg != false)) then {
                                    s := s ++ ", "
                                }
                            }
                            if (part.vararg != false) then {
                                s := s ++ "*" ++ part.vararg.value
                            }
                            s := s ++ ")"
                        }
                    }
                    if (node.dtype != false) then {
                        s := s ++ " -> {toGraceMatch(node.dtype, 0)}"
                    }
                    s := s ++ " \{"
                    for (node.body) do { mx ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(mx, depth + 1)
                    }
                    s := s ++ "\n" ++ spc ++ "\}"
                    return s
        }
        case { node : ClassNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "class {toGraceMatch(node.name, 0)}"
                    if (node.name.kind != "generic") then {
                        // TODO generics + new-style constructors aren't possible at the moment
                        s := s ++ "."
                        for (node.signature) do { part ->
                            s := s ++ part.name
                            if ((part.params.size > 0) | (part.vararg != false)) then {
                                s := s ++ "("
                                for (part.params.indices) do { pnr ->
                                    var p := part.params[pnr]
                                    s := s ++ toGraceMatch(p, depth + 1)
                                    if ((pnr < part.params.size) | (part.vararg != false)) then {
                                        s := s ++ ", "
                                    }
                                }
                                if (part.vararg != false) then {
                                    s := s ++ "*" ++ part.vararg.value
                                }
                                s := s ++ ")"
                            }
                        }
                    }
                    s := s ++ " \{"
                    for (node.value) do { mx ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(mx, depth + 1)
                    }
                    s := s ++ "\n" ++ spc ++ "\}"
                    return s
        }
        case { node : ObjectNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "object \{"
                    for (node.value) do { x ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(x, depth + 1)
                    }
                    s := s ++ "\n" ++ spc ++ "\}"
                    return s
        }
        case { node : ArrayNode ->
                    var s := "["
                    for (node.value.indices) do { i ->
                        s := s ++ toGraceMatch(node.value[i], 0)
                        if (i < node.value.size) then {
                            s := s ++ ", "
                        }
                    }
                    s := s ++ "]"
                    return s
        }
        case { node : DefDecNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "def {toGraceMatch(node.name, 0)}"
                    if (node.dtype.value != "Dynamic") then {
                        s := s ++ " : " ++ toGraceMatch(node.dtype, 0)
                    }
                    if (node.value != false) then {
                        s := s ++ " = " ++ toGraceMatch(node.value, depth)
                    }
                    return s
        }
        case { node : VarDecNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "var {toGraceMatch(node.name, 0)}"
                    if (node.dtype.value != "Dynamic") then {
                        s := s ++ " : " ++ toGraceMatch(node.dtype, 0)
                    }
                    if (node.value != false) then {
                        s := s ++ " := " ++ toGraceMatch(node.value, depth)
                    }
                    return s
        }
        case { node : ImportNode ->
                    return "import " ++ toGraceMatch(node.value, depth + 1)
        }
        case { node : ReturnNode ->
                    return "return " ++ toGraceMatch(node.value, depth)
        }
        case { node : InheritsNode ->
                    return "inherits {toGraceMatch(node.value, 0)}"
        }
        // The following cases are unused (bar GenericNode in a few
        // legacy test cases)
        case { node : GenericNode ->
                    var s := node.value.value ++ "<"
                    for (node.params.indices) do { i ->
                        s := s ++ toGraceMatch(node.params[i], 0)
                        if (i < node.params.size) then {
                            s := s ++ ", "
                        }
                    }
                    s := s ++ ">"
                    return s
        }
        case { node : ForNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "for ({toGraceMatch(node.value, 0)}) do "
                    s := s ++ toGraceMatch(node.body, depth)
                    return s
        }
        case { node : WhileNode ->
                    var spc := ""
                    for (0..(depth - 1)) do { i ->
                        spc := spc ++ "    "
                    }
                    var s := "while \{{toGraceMatch(node.value, depth + 1)}\} do \{"
                    for (node.body) do { x ->
                        s := s ++ "\n" ++ spc ++ "    " ++ toGraceMatch(x, depth + 1)
                    }
                    s := s ++ "\n" ++ spc ++ "\}"
                    return s
        }
        case { node : OctetsNode ->
                    return node.value
        }


    print("Unknown node type: {val.kind}")

    return ""
}
