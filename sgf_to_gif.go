package main

import (
	"fmt"
	"image"
	"image/gif"
	"image/color"
	"io/ioutil"
	"math"
	"os"
	"strconv"
	"strings"
)

const (
	STONE_WIDTH = 20
	MARGIN = 5
	DELAY = 40
	FINAL_DELAY = 400
)

var PALETTE = []color.Color{			// These should be in the same order as the constants below...
	color.RGBA{210, 175, 120, 255},
	color.Black,
	color.White,
}

const (									// Indexes for the colours above.
	BG = iota
	B
	W
)

// ------------------------------------------------

type Node struct {
	Props			map[string][]string
	Children		[]*Node
	Parent			*Node
}

func NewNode(parent *Node) *Node {
	node := new(Node)
	node.Props = make(map[string][]string)
	node.Parent = parent

	if parent != nil {
		parent.Children = append(parent.Children, node)
	}

	return node
}

func (self *Node) AddValue(key, value string) {

	for i := 0; i < len(self.Props[key]); i++ {
		if self.Props[key][i] == value {
			return
		}
	}

	self.Props[key] = append(self.Props[key], value)
}

func (self *Node) GetValue(key string) (value string, ok bool) {

	// Get the value for the key, on the assumption that there's only 1 value.

	list := self.Props[key]

	if len(list) == 0 {
		return "", false
	}

	return list[0], true
}

// ------------------------------------------------

type Colour int

const (
	EMPTY = Colour(iota)
	BLACK
	WHITE
)

type Board struct {
	State [][]Colour
}

func NewBoard(size int) *Board {

	board := new(Board)
	board.State = make([][]Colour, size)

	for x := 0; x < size; x++ {
		board.State[x] = make([]Colour, size)
	}

	return board
}

func (self *Board) Size() int {
	return len(self.State)
}

func (self *Board) PlayMove(colour Colour, x, y int) {

	if colour != BLACK && colour != WHITE {
		panic("PlayMove: colour != BLACK && colour != WHITE")
	}

	opponent := BLACK ; if colour == BLACK { opponent = WHITE }

	if x < 0 || x >= self.Size() || y < 0 || y > self.Size() {
		panic("PlayMove: off board")
	}

	self.State[x][y] = colour

	for _, point := range AdjacentPoints(x, y, self.Size()) {
		if self.State[point.X][point.Y] == opponent {
			if self.GroupHasLiberties(point.X, point.Y) == false {
				self.DestroyGroup(point.X, point.Y)
			}
		}
	}

	if self.GroupHasLiberties(x, y) == false {
		self.DestroyGroup(x, y)
	}
}

func (self *Board) GroupHasLiberties(x, y int) bool {
	touched := make(map[Point]bool)
	return self.group_has_liberties(x, y, touched)
}

func (self *Board) group_has_liberties(x, y int, touched map[Point]bool) bool {

	touched[Point{x, y}] = true

	colour := self.State[x][y]
	if colour != BLACK && colour != WHITE {
		panic("group_has_liberties: colour != BLACK && colour != WHITE")
	}

	for _, point := range AdjacentPoints(x, y, self.Size()) {
		if self.State[point.X][point.Y] == EMPTY {
			return true
		} else if self.State[point.X][point.Y] == colour {
			if touched[Point{point.X, point.Y}] == false {
				if self.group_has_liberties(point.X, point.Y, touched) {
					return true
				}
			}
		}
	}

	return false
}

func (self *Board) DestroyGroup(x, y int) {

	colour := self.State[x][y]
	if colour != BLACK && colour != WHITE {
		panic("DestroyGroup: colour != BLACK && colour != WHITE")
	}

	self.State[x][y] = EMPTY

	for _, point := range AdjacentPoints(x, y, self.Size()) {
		if self.State[point.X][point.Y] == colour {
			self.DestroyGroup(point.X, point.Y)
		}
	}
}

func (self *Board) UpdateFromNode(node *Node) {

	for _, foo := range node.Props["AB"] {
		point, ok := PointFromString(foo, self.Size())
		if ok { self.State[point.X][point.Y] = BLACK }
	}

	for _, foo := range node.Props["AW"] {
		point, ok := PointFromString(foo, self.Size())
		if ok { self.State[point.X][point.Y] = WHITE }
	}

	for _, foo := range node.Props["AE"] {
		point, ok := PointFromString(foo, self.Size())
		if ok { self.State[point.X][point.Y] = EMPTY }
	}

	for _, foo := range node.Props["B"] {
		point, ok := PointFromString(foo, self.Size())
		if ok {
			self.PlayMove(BLACK, point.X, point.Y)
		}
	}

	for _, foo := range node.Props["W"] {
		point, ok := PointFromString(foo, self.Size())
		if ok {
			self.PlayMove(WHITE, point.X, point.Y)
		}
	}
}

/*

func (self *Board) Dump() {

	fmt.Println()

	for y := 0; y < self.Size(); y++ {

		fmt.Printf("  ")

		for x := 0; x < self.Size(); x++ {
			switch self.State[x][y] {
			case EMPTY:
				fmt.Printf(". ")
			case BLACK:
				fmt.Printf("X ")
			case WHITE:
				fmt.Printf("O ")
			}
		}

		fmt.Printf("\n")
	}

	fmt.Println()
}

*/

// ------------------------------------------------

type Point struct {
	X	int
	Y	int
}

func AdjacentPoints(x, y, size int) []Point {

	var ret []Point

	possibles := []Point{
		Point{x - 1, y},
		Point{x + 1, y},
		Point{x, y - 1},
		Point{x, y + 1},
	}

	for _, point := range possibles {
		if point.X >= 0 && point.X < size {
			if point.Y >= 0 && point.Y < size {
				ret = append(ret, point)
			}
		}
	}

	return ret
}

func PointFromString(s string, size int) (Point, bool) {

	if len(s) < 2 {
		return Point{}, false
	}

	x := int(s[0]) - 97
	y := int(s[1]) - 97

	ok := false

	if x >= 0 && x < size && y >= 0 && y < size {
		ok = true
	}

	return Point{x, y}, ok
}

// ------------------------------------------------

func load_sgf_tree(sgf string, parent_of_local_root *Node) (*Node, int) {

	// FIXME: this is not unicode aware. Potential problems exist
	// if a unicode code point contains a meaningful character.

	var root *Node
	var node *Node

	var inside bool
	var value string
	var key string
	var keycomplete bool
	var chars_to_skip int
	var i int

	for i = 0; i < len(sgf); i++ {

		c := sgf[i]

		if chars_to_skip > 0 {
			chars_to_skip--
			continue
		}

		if inside {

			if c == '\\' {
				value += string('\\')
				value += string(sgf[i + 1])		// FIXME: can panic on bad SGF
				chars_to_skip = 1
			} else if c == ']' {
				inside = false
				if node == nil {
					panic("load_sgf_tree: node == nil after: else if c == ']'")
				}
				node.AddValue(key, value)
			} else {
				value += string(c)
			}

		} else {

			if c == '[' {
				value = ""
				inside = true
				keycomplete = true
			} else if c == '(' {
				if node == nil {
					panic("load_sgf_tree: node == nil after: else if c == '('")
				}
				_, chars_to_skip = load_sgf_tree(sgf[i + 1:], node)
			} else if c == ')' {
				if root == nil {
					panic("load_sgf_tree: root == nil after: else if c == ')'")
				}
				return root, i + 1
			} else if c == ';' {
				if node == nil {
					newnode := NewNode(parent_of_local_root)
					root = newnode
					node = newnode
				} else {
					newnode := NewNode(node)
					node = newnode
				}
			} else {
				if c >= 'A' && c <= 'Z' {
					if keycomplete {
						key = ""
						keycomplete = false
					}
					key += string(c)
				}
			}
		}
	}

	if root == nil {
		panic("load_sgf_tree: root == nil at function end")
	}

	return root, i + 1
}

func LoadSGF(sgf string) *Node {

	sgf = strings.TrimSpace(sgf)
	if sgf[0] == '(' {				// the load_sgf_tree() function assumes the
		sgf = sgf[1:]				// leading "(" has already been discarded.
	}

	root, _ := load_sgf_tree(sgf, nil)
	return root
}

// ------------------------------------------------

func main() {

	if len(os.Args) < 2 {
		fmt.Printf("Usage: %s <filename>\n", os.Args[0])
		return
	}

	sgf_bytes, err := ioutil.ReadFile(os.Args[1])

	if err != nil {
		fmt.Printf("%v\n", err)
		return
	}

	root := LoadSGF(string(sgf_bytes))

	size_string, _ := root.GetValue("SZ")
	size, _ := strconv.Atoi(size_string)
	if size <= 0 { size = 19 }

	board := NewBoard(size)

	node := root

	var out_gif gif.GIF

	for {
		board.UpdateFromNode(node)

		frame := frame_from_board(board)
		out_gif.Image = append(out_gif.Image, frame)
		out_gif.Delay = append(out_gif.Delay, DELAY)

		if len(node.Children) > 0 {
			node = node.Children[0]
		} else {
			break
		}
	}

	out_gif.Delay[len(out_gif.Delay) - 1] = FINAL_DELAY

	var filename string

	if len(os.Args[1]) > 4 && strings.HasSuffix(os.Args[1], ".sgf") {
		filename = os.Args[1][:len(os.Args[1]) - 4] + ".gif"
	} else {
		filename = os.Args[1] + ".gif"
	}

	save_gif(filename, &out_gif)
}

func frame_from_board(board *Board) *image.Paletted {

	size := (board.Size() * STONE_WIDTH) + (MARGIN * 2)

	rect := image.Rect(0, 0, size, size)
	c := image.NewPaletted(rect, PALETTE)

	// Background...

	for i := 0; i < size; i++ {
		for j := 0; j < size; j++ {
			c.SetColorIndex(i, j, BG)
		}
	}

	// Vertical lines...

	for x := 0; x < board.Size(); x++ {
		x1, y1 := image_xy(x, 0)
		_, y2 := image_xy(x, board.Size() - 1)

		i := x1

		for j := y1; j <= y2; j++ {
			c.SetColorIndex(i, j, B)
		}
	}

	// Horizontal lines...

	for y := 0; y < board.Size(); y++ {
		x1, y1 := image_xy(0, y)
		x2, _ := image_xy(board.Size() - 1, y)

		j := y1

		for i := x1; i <= x2; i++ {
			c.SetColorIndex(i, j, B)
		}
	}

	// Stones...

	for x := 0; x < board.Size(); x++ {

		for y := 0; y < board.Size(); y++ {

			x1, y1 := image_xy(x, y)

			if board.State[x][y] == BLACK {

				fcircle(c, B, x1, y1, STONE_WIDTH / 2)

			} else if board.State[x][y] == WHITE {

				fcircle(c, W, x1, y1, STONE_WIDTH / 2)
				circle(c, B, x1, y1, STONE_WIDTH / 2)

			} else if is_hoshi(x, y, board.Size()) {

				draw_hoshi(c, B, x1, y1)

			}
		}
	}

	return c
}

func is_hoshi(x, y, size int) bool {

	good_x, good_y := false, false

	if size >= 15 || x == y {
		if x == size / 2 { good_x = true }
		if y == size / 2 { good_y = true }
	}

	if size >= 12 {
		if x == 3 || x + 4 == size { good_x = true }
		if y == 3 || y + 4 == size { good_y = true }
	} else {
		if x == 2 || x + 3 == size { good_x = true }
		if y == 2 || y + 3 == size { good_y = true }
	}

	return good_x && good_y
}

func image_xy(x, y int) (int, int) {
	ret_x := x * STONE_WIDTH + MARGIN + (STONE_WIDTH / 2)
	ret_y := y * STONE_WIDTH + MARGIN + (STONE_WIDTH / 2)
	return ret_x, ret_y
}

func save_gif(path string, g *gif.GIF) {
	outfile, _ := os.Create(path)
	gif.EncodeAll(outfile, g)
}

func circle(c *image.Paletted, index uint8, x, y, radius int) {

	// I wrote this algorithm 15 years ago for C and can't remember how it works. But it does.

	var pyth float64
	var topline bool = true
	var lastiplusone int

	for j := radius - 1 ; j >= 0 ; j-- {
		for i := radius - 1 ; i >= 0 ; i-- {
			pyth = math.Sqrt(math.Pow(float64(i), 2) + math.Pow(float64(j), 2))
			if (pyth < float64(radius) - 0.5) {
				if topline {                    // i.e. if we're on the top (and, with mirroring, bottom) lines
					topline = false
					linehorizontal(c, index, x - i - 1, y - j - 1, x + i)
					linehorizontal(c, index, x - i - 1, y + j    , x + i)
					lastiplusone = i + 1
				} else {
					if lastiplusone == i + 1 {
						c.SetColorIndex(x - i - 1, y - j - 1, index)
						c.SetColorIndex(x + i    , y - j - 1, index)
						c.SetColorIndex(x - i - 1, y + j    , index)
						c.SetColorIndex(x + i    , y + j    , index)
					} else {
						linehorizontal(c, index, x - i - 1, y - j - 1, x - lastiplusone - 1)
						linehorizontal(c, index, x + lastiplusone , y - j - 1, x + i)
						linehorizontal(c, index, x - i - 1, y + j, x - lastiplusone - 1)
						linehorizontal(c, index, x + lastiplusone , y + j, x + i)
						lastiplusone = i + 1
					}
				}
				break
			}
		}
	}
}

func fcircle(c *image.Paletted, index uint8, x, y, radius int) {
	var pyth float64;

	for j := radius ; j >= 0 ; j-- {
		for i := radius ; i >= 0 ; i-- {
			pyth = math.Sqrt(math.Pow(float64(i), 2) + math.Pow(float64(j), 2));
			if (pyth < float64(radius) - 0.5) {
				linehorizontal(c, index, x - i - 1, y - j - 1, x + i)
				linehorizontal(c, index, x - i - 1, y + j, x + i)
				break
			}
		}
	}
}

func draw_hoshi(c *image.Paletted, index uint8, x, y int) {
	c.SetColorIndex(x - 1, y - 1, index)
	c.SetColorIndex(x - 1, y + 1, index)
	c.SetColorIndex(x + 1, y - 1, index)
	c.SetColorIndex(x + 1, y + 1, index)
}

func linehorizontal(c *image.Paletted, index uint8, x1, y1, x2 int) {
	for i := x1; i <= x2; i++ {
		c.SetColorIndex(i, y1, index)
	}
}
