import tkinter as tk
import os
import time
from tkinter import messagebox

class Conecta4App:
    def __init__(self, root):

        self.root = root
        self.root.title("Conecta 4")
        self.board = [[0 for _ in range(7)] for _ in range(6)]
        self.create_widgets()
        self.update_board()
        self.save_board_state()
        self.player_turn = True
        self.last_mod_time = 0
        

    def create_widgets(self):
        self.canvas = tk.Canvas(self.root, width=740, height=640, bg="blue")
        self.canvas.pack(padx=20, pady=20)
        self.canvas.bind("<Button-1>", self.handle_click)

    def handle_click(self, event):
        if not self.player_turn: 
            return
        
        col = (event.x - 20) // 100
        if 0 <= col < 7:
            self.make_move(col, 1)
            self.update_board()
            if self.check_winner(1):
                messagebox.showinfo("Conecta 4", "¡Jugador 1 gana!")
                self.reset_board()
                return

            self.save_board_state()
            self.player_turn = False  
            self.canvas.unbind("<Button-1>")  
            self.root.after(1000, self.wait_for_player2_move)

    def make_move(self, col, player):
        for row in reversed(range(6)):
            if self.board[row][col] == 0:
                self.board[row][col] = player
                break

    def update_board(self):
        self.canvas.delete("all")
        for row in range(6):
            for col in range(7):
                x0 = col * 100 + 20
                y0 = row * 100 + 20
                x1 = x0 + 100
                y1 = y0 + 100
                color = "white" if self.board[row][col] == 0 else ("red" if self.board[row][col] == 1 else "yellow")
                self.canvas.create_oval(x0, y0, x1, y1, fill=color)

    def save_board_state(self):
        with open("board_state.txt", "w") as file:
            # Iterar por columnas (de izquierda a derecha)
            for col in range(len(self.board[0])):  # Iterar sobre las columnas del tablero
                # Recoger los valores de la columna de abajo hacia arriba
                column_values = [str(self.board[row][col]) for row in reversed(range(len(self.board)))]
                file.write(" ".join(column_values) + "\n")
        
            
        
    def wait_for_player2_move(self):
        changed = False
        while not changed:
            new_mod_time = os.path.getmtime("board_state.txt")
            changed = new_mod_time != self.last_mod_time
            if changed:
                self.load_board_state()
                self.update_board()
                self.last_mod_time = new_mod_time 
                if self.check_winner(2):
                    messagebox.showinfo("Conecta 4", "¡Jugador 2 gana!")
                    self.reset_board()
                self.player_turn = True  
                self.canvas.bind("<Button-1>", self.handle_click)  
                break


    def load_board_state(self):
        with open("board_state.txt", "r") as file:
            lines = [line.strip().split() for line in file if line.strip()]  # Leer y limpiar líneas vacías

        # Verificar que el archivo tenga dimensiones válidas
        expected_rows = 6
        expected_cols = 7

        if len(lines) != expected_cols or any(len(line) != expected_rows for line in lines):
            raise ValueError("El archivo board_state.txt no tiene el formato correcto.")

        # Inicializar el tablero con dimensiones correctas (6 filas, 7 columnas)
        self.board = [[0 for _ in range(expected_cols)] for _ in range(expected_rows)]

        # Rellenar el tablero con las columnas invertidas verticalmente
        for col in range(expected_cols):
            for row in range(expected_rows):
                # Asignar valores de la columna del archivo a las filas del tablero (invertidas)
                self.board[expected_rows - 1 - row][col] = int(lines[col][row])

        self.last_mod_time = os.path.getmtime("board_state.txt")



    def check_winner(self, player):
        for row in range(6):
            for col in range(7):
                if self.check_line(player, row, col, 1, 0) or \
                   self.check_line(player, row, col, 0, 1) or \
                   self.check_line(player, row, col, 1, 1) or \
                   self.check_line(player, row, col, 1, -1):
                    return True
        return False

    def check_line(self, player, row, col, delta_row, delta_col):
        count = 0
        for i in range(4):
            r = row + i * delta_row
            c = col + i * delta_col
            if 0 <= r < 6 and 0 <= c < 7 and self.board[r][c] == player:
                count += 1
            else:
                break
        return count == 4

    def reset_board(self):
        self.board = [[0 for _ in range(7)] for _ in range(6)]
        self.update_board()
        self.player_turn = True  # Asegurarse de que empieza el Jugador 1
        self.canvas.bind("<Button-1>", self.handle_click)  

if __name__ == "__main__":
    root = tk.Tk()
    app = Conecta4App(root)
    root.mainloop()
