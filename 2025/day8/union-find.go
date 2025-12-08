package day8

type DisjoinSets struct {
	parent []int
	size   []int
}

func Init(size int) DisjoinSets {
	p := make([]int, size)
	for i := range p {
		p[i] = i
	}
	s := make([]int, size)
	for i := range s {
		s[i] = 1
	}

	return DisjoinSets{
		parent: p,
		size:   s,
	}
}

func (dj *DisjoinSets) Find(i int) int {
	root := dj.parent[i]

	if dj.parent[root] != root {
		dj.parent[i] = dj.Find(root)
		return dj.parent[i]
	}

	return root
}

func (dj *DisjoinSets) Union(i, j int) {
	iRep := dj.Find(i)
	jRep := dj.Find(j)

	if iRep == jRep {
		return
	}

	iSize := dj.size[iRep]
	jSize := dj.size[jRep]

	if iSize < jSize {
		dj.parent[iRep] = jRep
		dj.size[jRep] += dj.size[iRep]
		// Since the i representive is no longer active, we can change the size to 1
		dj.size[iRep] = 0
	} else {
		dj.parent[jRep] = iRep
		dj.size[iRep] += dj.size[jRep]
		// Since the i representive is no longer active, we can change the size to 1
		dj.size[jRep] = 0
	}
}

func (dj *DisjoinSets) Sizes() []int {
	return dj.size
}
