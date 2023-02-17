import tkinter as tk
from tkinter import filedialog
from compiler import compile_c_to_mips

class Application(tk.Frame):
    def __init__(self, master=None):
        super().__init__(master)
        self.master = master
        self.pack()
        self.create_widgets()

    def create_widgets(self):
        self.file_label = tk.Label(self, text="Choose a file to compile:")
        self.file_label.pack()

        self.file_button = tk.Button(self, text="Browse", command=self.browse_file)
        self.file_button.pack()

        self.compile_button = tk.Button(self, text="Compile", command=self.compile_file)
        self.compile_button.pack()

        self.output_label = tk.Label(self, text="Output:")
        self.output_label.pack()

        self.output_text = tk.Text(self)
        self.output_text.pack()

    def browse_file(self):
        filename = filedialog.askopenfilename()
        if filename:
            self.file_label.config(text=filename)

    def compile_file(self):
        filename = self.file_label.cget("text")
        try:
            mips_code = compile_to_mips(filename)
            self.output_text.delete("1.0", tk.END)
            self.output_text.insert(tk.END, mips_code)
        except Exception as e:
            self.output_text.delete("1.0", tk.END)
            self.output_text.insert(tk.END, str(e))

root = tk.Tk()
app = Application(master=root)
app.mainloop()