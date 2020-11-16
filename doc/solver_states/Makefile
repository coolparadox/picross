all: picross_solver_states.png
.PHONY: all

picross_solver_states.png: picross_solver_states.dot
	dot -Tpng $< >$@

clean:
	$(RM) *.png
