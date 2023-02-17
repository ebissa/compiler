import tkinter as tk
from lexer import Lexer
from parser import Parser
from symbol_table import SymbolTable
from compiler import generate_mips, run_mips

class CompilerGUI:
    def __init__(self, master):
        self.master = master
        master.title("Mini C Compiler")

        self.input_label = tk.Label(master, text="Input Program:")
        self.input_label.pack()

        self.input_text = tk.Text(master, width=50, height=10)
        self.input_text.pack()

        self.compile_button = tk.Button(master, text="Compile", command=self.compile_program)
        self.compile_button.pack()

        self.output_label = tk.Label(master, text="Output:")
        self.output_label.pack()

        self.output_text = tk.Text(master, width=50, height=10)
        self.output_text.pack()

    def compile_program(self):
        # Get the input program from the text box
        program = self.input_text.get("1.0", tk.END)

        # Create a lexer and parser
        lexer = Lexer()
        parser = Parser()

        # Tokenize and parse the program
        tokens = lexer.tokenize(program)
        ast = parser.parse(tokens)

        # Create a symbol table
        symbol_table = SymbolTable()

        # Generate the MIPS code for the program
        mips_code = generate_mips(ast, symbol_table)

        # Run the MIPS code
        output = run_mips(mips_code)

        # Display the output in the text box
        self.output_text.delete("1.0", tk.END)
        self.output_text.insert(tk.END, output)

root = tk.Tk()
gui = CompilerGUI(root)
root.mainloop()

