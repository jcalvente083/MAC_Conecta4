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

    def create_widgets(self):
        self.canvas = tk.Canvas(self.root, width=740, height=640, bg="blue")  # Añadido margen
        self.canvas.pack(padx=20, pady=20)  # Añadido margen
        self.canvas.bind("<Button-1>", self.handle_click)

    def handle_click(self, event):
        col = (event.x - 20) // 100  # Ajuste por el margen
        if 0 <= col < 7:  # Asegurarse de que el clic esté dentro del tablero
            self.make_move(col, 1)  # Movimiento del jugador 1
            self.update_board()
            if self.check_winner(1):
                messagebox.showinfo("Conecta 4", "¡Jugador 1 gana!")
                self.reset_board()
                return
            self.save_board_state()
            self.root.after(1000, self.wait_for_player2_move)  # Esperar 1 segundo antes de comprobar el movimiento del jugador 2

    def make_move(self, col, player):
        for row in reversed(range(6)):
            if self.board[row][col] == 0:
                self.board[row][col] = player
                break

    def update_board(self):
        self.canvas.delete("all")
        for row in range(6):
            for col in range(7):
                x0 = col * 100 + 20  # Ajuste por el margen
                y0 = row * 100 + 20  # Ajuste por el margen
                x1 = x0 + 100
                y1 = y0 + 100
                color = "white" if self.board[row][col] == 0 else ("red" if self.board[row][col] == 1 else "yellow")
                self.canvas.create_oval(x0, y0, x1, y1, fill=color)

    def save_board_state(self):
        with open("board_state.txt", "w") as file:
            for row in self.board:
                file.write(" ".join(map(str, row)) + "\n")

    def wait_for_player2_move(self):
        last_mod_time = os.path.getmtime("board_state.txt")
        while True:
            time.sleep(1)  # Esperar 1 segundo antes de comprobar de nuevo
            new_mod_time = os.path.getmtime("board_state.txt")
            if new_mod_time != last_mod_time:
                self.load_board_state()
                self.update_board()
                if self.check_winner(2):
                    messagebox.showinfo("Conecta 4", "¡Jugador 2 gana!")
                    self.reset_board()
                break

    def load_board_state(self):
        with open("board_state.txt", "r") as file:
            for i, line in enumerate(file):
                self.board[i] = list(map(int, line.strip().split()))

    def check_winner(self, player):
        # Comprobar filas, columnas y diagonales
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

if __name__ == "__main__":
    root = tk.Tk()
    app = Conecta4App(root)
    root.mainloop()