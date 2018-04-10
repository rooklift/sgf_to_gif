package main

import (
	"fmt"
	"image"
	"image/draw"
	"image/gif"
	"image/color/palette"
	"os"
	"strconv"
	"strings"

	"github.com/fohristiwhirl/ezcanvas"
)

const (
	EMPTY = Colour(0)
	BLACK = Colour(1)
	WHITE = Colour(2)

	STONE_WIDTH = 20
	MARGIN = 5
)

type Colour int

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

	const TEST = `
		(;SZ[19]FF[3]
		PW[Inagaki Kanetaro]
		PB[Saka Hakusuisanjin]
		EV[Teaching game]
		DT[Published 1921]
		OH[3]
		HA[3]
		RE[W+5]
		US[GoGoD95]
		AB[dp][pd][dd]
		;W[qp];B[oq];W[mq];B[mp];W[lp];B[np];W[kq];B[pn];W[qn];B[qm];W[pm];B[qo]
		;W[rn];B[on];W[ro];B[pl];W[ed];B[ee];W[fe];B[om];W[dc];B[ec];W[fd];B[cc]
		;W[db];B[cb];W[de];B[cd];W[eb];B[cf];W[ef];B[kc];W[cn];B[ci];W[fq];B[fp]
		;W[gp];B[fo];W[eq];B[dq];W[bo];B[cl];W[dl];B[gq];W[gr];B[hq];W[hr];B[iq]
		;W[ir];B[dr];W[ck];B[ho];W[nc];B[gb];W[gc];B[hb];W[qf];B[pf];W[pg];B[qe]
		;W[of];B[pe];W[ne];B[qg];W[ph];B[md];W[nd];B[pb];W[lb];B[id];W[jb];B[kb]
		;W[jc];B[jd];W[ka];B[ic];W[kd];B[ke];W[lc];B[jf];W[jh];B[ig];W[fi];B[bk]
		;W[bl];B[bj];W[hi];B[kg];W[rf];B[rg];W[rh];B[re];W[qh];B[sf];W[gn];B[go]
		;W[qr];B[lo];W[ko];B[ln];W[kn];B[lm];W[lf];B[kh];W[bq];B[ep];W[fr];B[br]
		;W[le];B[ji];W[kj];B[ih];W[hf];B[fb];W[fc];B[jj];W[kl];B[gh];W[fh];B[cm]
		;W[en];B[bm];W[jk];B[hj];W[gi];B[lj];W[mk];B[ml];W[nl];B[nk];W[ll];B[mm]
		;W[mj];B[nj];W[ni];B[oi];W[mi];B[fa];W[ca];B[ba];W[da];B[ea];W[ec];B[ib]
		;W[ce];B[be];W[bf];B[df];W[ee];B[bg];W[bd];B[af];W[bb];B[dn];W[do];B[dm]
		;W[eo];B[fl];W[em];B[el];W[gm];B[hl];W[in];B[hn];W[hm];B[im];W[il];B[gl]
		;W[jm];B[cp];W[co];B[bp];W[ap];B[ar];W[ok];B[oj];W[pk];B[qk];W[pj];B[ol]
		;W[or]
		)
	`

	root := LoadSGF(TEST)

	size_string, _ := root.GetValue("SZ")
	size, _ := strconv.Atoi(size_string)
	if size <= 0 { size = 19 }

	board := NewBoard(size)

	node := root

	var out_gif gif.GIF

	for {
		board.UpdateFromNode(node)

		ez_frame := ez_frame_from_board(board)
		out_gif.Image = append(out_gif.Image, ez_to_paletted(ez_frame))
		out_gif.Delay = append(out_gif.Delay, 20)

		if len(node.Children) > 0 {
			node = node.Children[0]
		} else {
			break
		}
	}

	save_gif("foo.gif", &out_gif)
}

func ez_frame_from_board(board *Board) *ezcanvas.Canvas {

	size := (board.Size() * STONE_WIDTH) + (MARGIN * 2)

	c := ezcanvas.NewCanvas(size, size)
	c.Clear(210, 175, 120)

	for x := 0; x < board.Size(); x++ {
		x1, y1 := image_xy(x, 0)
		x2, y2 := image_xy(x, board.Size() - 1)
		c.Line(0, 0, 0, ezcanvas.SET, x1, y1, x2, y2)
	}

	for y := 0; y < board.Size(); y++ {
		x1, y1 := image_xy(0, y)
		x2, y2 := image_xy(board.Size() - 1, y)
		c.Line(0, 0, 0, ezcanvas.SET, x1, y1, x2, y2)
	}

	for x := 0; x < board.Size(); x++ {

		for y := 0; y < board.Size(); y++ {

			x1, y1 := image_xy(x, y)

			if board.State[x][y] == BLACK {
				c.Fcircle(0, 0, 0, ezcanvas.SET, x1, y1, STONE_WIDTH / 2)
			} else if board.State[x][y] == WHITE {
				c.Fcircle(255, 255, 255, ezcanvas.SET, x1, y1, STONE_WIDTH / 2)
				c.Circle(0, 0, 0, ezcanvas.SET, x1, y1, STONE_WIDTH / 2)
			}
		}
	}

	return c
}

func image_xy(x, y int) (int, int) {
	ret_x := x * STONE_WIDTH + MARGIN + (STONE_WIDTH / 2)
	ret_y := y * STONE_WIDTH + MARGIN + (STONE_WIDTH / 2)
	return ret_x, ret_y
}

func ez_to_paletted(c *ezcanvas.Canvas) *image.Paletted {
	field := c.Field()
	p := image.NewPaletted(field.Bounds(), palette.Plan9)
	draw.Draw(p, p.Rect, field, field.Bounds().Min, draw.Over)
	return p
}

func save_gif(path string, g *gif.GIF) {
	outfile, _ := os.Create(path)
	gif.EncodeAll(outfile, g)
}
