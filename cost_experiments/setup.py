#!/usr/bin/env python3

from csv import writer


if __name__ == "__main__":

    with open("./results/results.csv", "w") as csv_file:
        csv_writer = writer(csv_file)
        csv_writer.writerow(["Empty Instruction", "Unop: Not", "Unop: Minus", "Binop: Plus (Int)", "Binop: Plus (String)", "Binop: Minus", "Binop: Mult", "Binop: Lt", "Binop: Le", "Binop: Gt", "Binop: Ge", "Binop: Eq", "Binop: Ne", "Binop: And", "Binop: Or", "Func Call: User func (Init)", "Func Call: Print", "Func Call: Input",
                            "Func Call: Open", "Func Call: Read", "Func Call: Write", "Func Call: Append", "Expr: Var Read", "Var Statement: Var Non-init", "Var Statement: Var Init", "Var Statement: Var Decl", "Var Statement: Var Assign", "Var Statement: Post Inc", "Var Statement: Post Dec", "For Loop (Init)", "For Each (Init)", "If Statement (Init)"])
