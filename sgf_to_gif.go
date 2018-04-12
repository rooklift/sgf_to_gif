package main

import (
	"flag"
	"fmt"
	"image"
	"image/gif"
	"image/color"
	"io/ioutil"
	"math"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"./chars"
)

type Config struct {
	StoneWidth		int
	Margin			int
	Delay			int
	FinalDelay		int
	NoCoords		bool
	NoNumbers		bool

	SplitString		string
	Splits			map[int]bool
	Splitting		bool
}

var cfg Config

func init() {
	flag.IntVar(&cfg.StoneWidth, "s", 22, "stone width")
	flag.IntVar(&cfg.Margin, "m", 5, "outer margin")
	flag.IntVar(&cfg.Delay, "d", 50, "delay")
	flag.IntVar(&cfg.FinalDelay, "f", 500, "final delay")
	flag.BoolVar(&cfg.NoCoords, "c", false, "disable coordinates")
	flag.BoolVar(&cfg.NoNumbers, "n", false, "disable move numbers")
	flag.StringVar(&cfg.SplitString, "t", "", "split files after these moves (e.g. \"50 100 150\")")
	flag.Parse()

	if cfg.StoneWidth < 4 { cfg.StoneWidth = 4 }
	if cfg.StoneWidth > 64 { cfg.StoneWidth = 64 }
	if cfg.Margin < 0 { cfg.Margin = 0 }
	if cfg.Margin > 256 { cfg.Margin = 256 }

	cfg.Splits = make(map[int]bool)

	if cfg.SplitString != "" {
		parts := strings.Fields(cfg.SplitString)
		for _, s := range parts {
			n, _ := strconv.Atoi(s)
			if n > 0 {
				cfg.Splits[n] = true
				cfg.Splitting = true
			}
		}
	}
}

var PALETTE = []color.Color{			// These should be in the same order as the constants below...
	color.RGBA{210, 175, 120, 255},
	color.Black,
	color.White,
}

const (									// Indexes for the colours above.
	BG = uint8(iota)
	B
	W
)

type Colour int

const (
	EMPTY = Colour(iota)
	BLACK
	WHITE
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

func (self *Node) MoveCoords(size int) (x, y int, colour Colour, ok bool) {

	for _, foo := range self.Props["B"] {
		point, ok := PointFromString(foo, size)
		if ok {
			return point.X, point.Y, BLACK, true
		}
	}

	for _, foo := range self.Props["W"] {
		point, ok := PointFromString(foo, size)
		if ok {
			return point.X, point.Y, WHITE, true
		}
	}

	return -1, -1, EMPTY, false
}

// ------------------------------------------------

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

func (self *Board) UpdateFromNode(node *Node) int {

	moves_made := 0

	// Add stones: AB / AW / AE

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

	// Play move: B / W

	for _, foo := range node.Props["B"] {
		point, ok := PointFromString(foo, self.Size())
		if ok { self.PlayMove(BLACK, point.X, point.Y) }
		moves_made++	// Includes passes
	}

	for _, foo := range node.Props["W"] {
		point, ok := PointFromString(foo, self.Size())
		if ok { self.PlayMove(WHITE, point.X, point.Y) }
		moves_made++	// Includes passes
	}

	return moves_made
}

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

	for i := 0; i < len(sgf); i++ {

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
				return root, i + 1		// Return characters read.
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

	return root, len(sgf)			// Return characters read.
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
		fmt.Printf("Usage: %s <options> <filename>\n", filepath.Base(os.Args[0]))
		return
	}

	// Filename should be at end of args...

	infilename := os.Args[len(os.Args) - 1]

	if _, err := os.Stat(infilename); err != nil {
		fmt.Printf("File %s doesn't exist.\n", infilename)
		fmt.Printf("Usage: %s <options> <filename>\n", filepath.Base(os.Args[0]))
		return
	}

	sgf_bytes, err := ioutil.ReadFile(infilename)

	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}

	root := LoadSGF(string(sgf_bytes))

	size_string, _ := root.GetValue("SZ")
	size, _ := strconv.Atoi(size_string)
	if size <= 0 { size = 19 }

	board := NewBoard(size)
	prev_board := NewBoard(size)

	node := root

	image_width := cfg.Margin + (board.Size() * cfg.StoneWidth) + cfg.Margin
	image_height := cfg.Margin + (board.Size() * cfg.StoneWidth) + cfg.Margin

	if cfg.NoCoords == false {
		image_height += cfg.StoneWidth
		image_width += cfg.StoneWidth
	}

	gif_config := image.Config{
		Width: image_width,
		Height: image_height,
	}

	x_offset := cfg.Margin		// Where the changeable part
	y_offset := cfg.Margin		// actually starts in the image.

	total_moves := 0
	moves_last_update := 0
	finished := false

	for {

		out_gif := gif.GIF{Config: gif_config}

		for {
			moves_last_update = board.UpdateFromNode(node)
			total_moves += moves_last_update
			if node.Parent != nil { prev_board.UpdateFromNode(node.Parent) }

			var canvas *image.Paletted

			if len(out_gif.Image) == 0 {

				canvas = full_canvas(image_width, image_height)
				draw_board(canvas, board, x_offset, y_offset)
				if cfg.NoCoords == false { draw_coords(canvas, board, x_offset, y_offset) }

			} else {

				canvas = partial_canvas(board, prev_board, x_offset, y_offset)
				draw_board(canvas, board, x_offset, y_offset)

			}

			added_move_number := false		// Can be false even if we're generally adding them; e.g. if a pass.

			move_x, move_y, colour, ok := node.MoveCoords(board.Size())

			if ok && cfg.NoNumbers == false && (len(out_gif.Image) > 0 || total_moves == 1) {
				x1, y1 := image_xy(move_x, move_y)
				cindex := B ; if colour == BLACK { cindex = W }
				s := fmt.Sprintf("%d", total_moves)
				draw_text(canvas, cindex, s, x1 + x_offset, y1 + y_offset)
				added_move_number = true
			}

			out_gif.Image = append(out_gif.Image, canvas)
			out_gif.Delay = append(out_gif.Delay, cfg.Delay)
			out_gif.Disposal = append(out_gif.Disposal, gif.DisposalNone)

			if added_move_number {
				c := one_stone_canvas(move_x, move_y, x_offset, y_offset)
				draw_board(c, board, x_offset, y_offset)
				out_gif.Image = append(out_gif.Image, c)
				out_gif.Delay = append(out_gif.Delay, 0)
				out_gif.Disposal = append(out_gif.Disposal, gif.DisposalNone)
			}

			if len(node.Children) > 0 {
				node = node.Children[0]
			} else {
				finished = true
				break
			}

			if cfg.Splits[total_moves] {
				delete(cfg.Splits, total_moves)		// Delete from the map so it doesn't trigger again after we back up 1.
				break
			}
		}

		// Fix up some stuff and save...

		out_gif.Delay[len(out_gif.Delay) - 1] = cfg.FinalDelay

		var filename string

		if cfg.Splitting {

			if len(infilename) > 4 && strings.HasSuffix(infilename, ".sgf") {
				filename = infilename[:len(infilename) - 4] + fmt.Sprintf(".%d.gif", total_moves)
			} else {
				filename = infilename + fmt.Sprintf(".%d.gif", total_moves)
			}

		} else {

			if len(infilename) > 4 && strings.HasSuffix(infilename, ".sgf") {
				filename = infilename[:len(infilename) - 4] + ".gif"
			} else {
				filename = infilename + ".gif"
			}

		}

		save_gif(filename, &out_gif)

		if finished {
			break
		}

		// So we will re-enter the main loop to do a new file (because we're splitting).
		// Therefore back up one so that the board doesn't change. Rather, we start the new
		// file in the same position as the last frame of the old file.

		if node.Parent != nil {
			node = node.Parent
			total_moves -= moves_last_update
		}
	}
}

func draw_board(c *image.Paletted, board *Board, x_offset, y_offset int) {

	// Fill the entire canvas with background colour, then
	// draw the board. We don't care about the boundaries,
	// i.e. we will often be outside them. Whatever.

	for i := c.Rect.Min.X; i < c.Rect.Max.X; i++ {
		for j := c.Rect.Min.Y; j < c.Rect.Max.Y; j++ {
			c.SetColorIndex(i, j, BG)
		}
	}

	// Vertical lines...

	for x := 0; x < board.Size(); x++ {
		x1, y1 := image_xy(x, 0)
		_, y2 := image_xy(x, board.Size() - 1)

		for j := y1; j <= y2; j++ {
			c.SetColorIndex(x1 + x_offset, j + y_offset, B)
		}
	}

	// Horizontal lines...

	for y := 0; y < board.Size(); y++ {
		x1, y1 := image_xy(0, y)
		x2, _ := image_xy(board.Size() - 1, y)

		for i := x1; i <= x2; i++ {
			c.SetColorIndex(i + x_offset, y1 + y_offset, B)
		}
	}

	// Hoshi...

	for x := 0; x < board.Size(); x++ {
		for y := 0; y < board.Size(); y++ {
			if is_hoshi(x, y, board.Size()) {
				x1, y1 := image_xy(x, y)
				hoshi(c, B, x1 + x_offset, y1 + y_offset)
			}
		}
	}

	// Stones...

	for x := 0; x < board.Size(); x++ {

		for y := 0; y < board.Size(); y++ {

			x1, y1 := image_xy(x, y)

			if board.State[x][y] == BLACK {

				fcircle(c, B, x1 + x_offset, y1 + y_offset, cfg.StoneWidth / 2)

			} else if board.State[x][y] == WHITE {

				fcircle(c, W, x1 + x_offset, y1 + y_offset, cfg.StoneWidth / 2)
				circle(c, B, x1 + x_offset, y1 + y_offset, cfg.StoneWidth / 2)

			}
		}
	}
}

func draw_coords(c *image.Paletted, board *Board, x_offset, y_offset int) {

	// FIXME: fail gracefully if size > 25

	letters := "ABCDEFGHJKLMNOPQRSTUVWXYZ"

	for x := 0; x < board.Size(); x++ {
		x1, y1 := image_xy(x, board.Size())
		s := string(letters[x])
		draw_text(c, B, s, x1 + x_offset, y1 + y_offset)
	}

	for y := 0; y < board.Size(); y++ {
		x1, y1 := image_xy(board.Size(), board.Size() - y - 1)
		s := fmt.Sprintf("%d", y + 1)
		draw_text(c, B, s, x1 + x_offset, y1 + y_offset)
	}
}

func draw_text(c *image.Paletted, cindex uint8, s string, x, y int) {
	points := chars.Points(s)
	for _, point := range points {
		c.SetColorIndex(x + point.X, y + point.Y, cindex)
	}
}

func full_canvas(image_width, image_height int) *image.Paletted {
	rect := image.Rect(0, 0, image_width, image_height)
	return image.NewPaletted(rect, PALETTE)
}

func partial_canvas(board *Board, previous *Board, x_offset, y_offset int) *image.Paletted {

	// Given 2 boards, return a canvas for the part of the image that's getting updated.

	logical_left, logical_top, logical_right, logical_bottom := relevant_region(board, previous)

	rect := image.Rect(
		logical_left * cfg.StoneWidth + x_offset,
		logical_top * cfg.StoneWidth + y_offset,
		(logical_right + 1) * cfg.StoneWidth + x_offset,
		(logical_bottom + 1) * cfg.StoneWidth + y_offset,
	)

	return image.NewPaletted(rect, PALETTE)
}

func one_stone_canvas(move_x, move_y, x_offset, y_offset int) *image.Paletted {
	rect := image.Rect(
		move_x * cfg.StoneWidth + x_offset,
		move_y * cfg.StoneWidth + y_offset,
		(move_x + 1) * cfg.StoneWidth + x_offset,
		(move_y + 1) * cfg.StoneWidth + y_offset,
	)
	return image.NewPaletted(rect, PALETTE)
}

func relevant_region(one, two *Board) (int, int, int, int) {

	// Returns an INCLUSIVE, logical rectangle (game coords).
	// For most nodes (with a single move and no captures),
	// it will be the case that top == bottom and left == right.

	left := one.Size()		// i.e. out of bounds
	top := one.Size()
	right := -1
	bottom := -1

	for x := 0; x < one.Size(); x++ {
		for y := 0; y < one.Size(); y++ {
			if one.State[x][y] != two.State[x][y] {
				if left > x { left = x }
				if top > y { top = y }
				if right < x { right = x }
				if bottom < y { bottom = y }
			}
		}
	}

	if left > right || top > bottom {		// Force the caller to make some non-zero sized frame.
		return 0, 0, 0, 0
	}

	return left, top, right, bottom
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

	// Result has no margins or anything. This is fine.

	ret_x := (x * cfg.StoneWidth) + (cfg.StoneWidth / 2)
	ret_y := (y * cfg.StoneWidth) + (cfg.StoneWidth / 2)
	return ret_x, ret_y
}

func save_gif(path string, g *gif.GIF) {
	outfile, err := os.Create(path)
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}
	err = gif.EncodeAll(outfile, g)
	if err != nil {
		panic(fmt.Sprintf("%v", err))
	}
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

func hoshi(c *image.Paletted, index uint8, x, y int) {
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
