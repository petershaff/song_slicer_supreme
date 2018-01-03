import numpy as np
import scipy as sp

def bin_str_ent(v):
	if len(v) == 0:
		return(0)

	N = len(v)
	n = sum(v)
	p = n/N
	q = 1 - p
	return( -p*np.log(p) - q*np.log(q) )

class node(list):
        def __init__(self, dat, name, term):
                self.dat = dat
		self.name = name
		self.term = term

		self.N = len( dat['labels'] ) 
		self.attributes = dat.dtype.names
		self.h_v = bin_str_ent( dat['labels'] )

	def attr_choose(self):
		entrop_dict = {} 
		split_dict = {}	
		for attr in self.attributes:
			attr_vals = self.dat[attr]
		
			curr_split = attr_vals[0]
			p = len( dat[ attr <= curr_split ][attr] )/self.N
			q = 1 - p
			h_yx = p*bin_str_ent( dat[ attr <= curr_split ]['labels'] ) + q*bin_str_ent( dat[ attr > x ]['labels'] )
			for x in attr_vals[1:]:
				p = len( dat[ attr <= x ][attr] )/self.N
				q = 1 - p
				
				h_yx_cand = p*bin_str_ent( dat[ attr <= x ]['labels'] ) + q*bin_str_ent( dat[ attr > x ]['labels'] )
				if h_yx_cand < h_yx:
					curr_split = x
					h_yx = h_yx_cand		
			
			entrop_dict[attr] = h_yx			
			split_dict[attr] = curr_split	

		self.attr = min(entrop_dict, key=entrop_dict.get)
		self.split_val = spit_dict[self.attribute]

	def split(self):
		if len( np.unique( dat[ self.attr <= self.split_val ]['labels'] ) ) == 1:
					self.term = 1
					self.label = dat['labels'][0]
		
		elif len( np.unique( dat[ self.attr > self.split_val ]['labels'] ) ) == 1:
					self.term = 1
					self.label = dat['labels'][0]
		else:
			node0 = node( dat[ self.attr <= self.split_val ], 0, 0 )
			node1 = node( dat[ self.attr >  self.split_val ], 1, 0 )
			self.append( node0 )
			self.append( node1 )



stop = 0
branches = [0,1]
while stop == 0:
	tree = node(dat, 0, 0)
	curr_curr
	map = [0]
	tree.attr_choose()
	tree.split()

 	for i in branches:
	


