# GLOSSARY
################################################################################
#   * Term
# Either terminal or non-terminal symbol.
#   * Production
# A right-hand side of a production rule; formally, a sequence of terms.
#   * Rule
# A set of all possible production rules, grouped by left-hand side.
#
# For example, in grammar:
#   S  -> NP VP
#   NP -> D N
#   NP -> John
#   D  -> the
#   D  -> a
#   N  -> cat
#   N  -> dog
#   ...
#
# "S", "NP", "VP", "D", "N", "John", "the", "a", "cat", "god"
#   are terms.
# [ "NP, "VP" ], [ "D", "N" ], [ "John" ], [ "the" ], [ "a" ], ...
#   are productions for productions rules (1) and (2) respectivelly.
# ("S", [ [ "NP" "VP" ] ]), ("NP", [ [ "D", "N" ], [ "John"] ]), ...
#   are rules.
   
class Production(object):
    def __init__(self, *terms):
        self.terms = terms

    def __len__(self):
        return len(self.terms)

    def __getitem__(self, index):
        return self.terms[index]

    def __iter__(self):
        return iter(self.terms)

    def __repr__(self):
        return " ".join(str(t) for t in self.terms)

    def __eq__(self, other):
        if not isinstance(other, Production):
            return False
        return self.terms == other.terms

    def __ne__(self, other):
        return not (self == other)

    def __hash__(self):
        return hash(self.terms)

class Rule(object):
    def __init__(self, name, *productions):
        self.name = name
        self.productions = list(productions)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "%s -> %s" % (
            self.name,
            " | ".join(repr(p) for p in self.productions))

    def add(self, *productions):
        self.productions.extend(productions)

# State is a 3-tuple of a dotted rule, start column and end column.
class State(object):
    # A dotted rule is represented as a (name, production, dot_index) 3-tuple.
    def __init__(self, name, production, dot_index, start_column, end_column = None):
        self.name = name
        self.production = production
        self.dot_index = dot_index

        self.start_column = start_column
        self.end_column = end_column
        
        self.rules = [ term for term in self.production if isinstance(term, Rule) ]

    def __repr__(self):
        terms = [ str(term) for term in self.production ]
        terms.insert(self.dot_index, "$")

        return "%-5s -> %-23s [%2s-%-2s]" % (
            self.name,
            " ".join(terms),
            self.start_column,
            self.end_column)

    def __eq__(self, other):
        return \
            (self.name,  self.production,  self.dot_index,  self.start_column) == \
            (other.name, other.production, other.dot_index, other.start_column)

    def __ne__(self, other):
        return not (self == other)

    # Note that objects are hashed by (name, production), but not the whole state.
    def __hash__(self):
        return hash((self.name, self.production))

    def is_completed(self):
        return self.dot_index >= len(self.production)

    def get_next_term(self):
        if self.is_completed():
            return None
        return self.production[self.dot_index]

# Column is a list of states in a chart table.
class Column(object):
    def __init__(self, index, token):
        self.index = index
        self.token = token

        self.states = []
        self._unique_states = set()

    def __str__(self):
        return str(self.index)

    def __len__(self):
        return len(self.states)

    def __iter__(self):
        return iter(self.states)

    def __getitem__(self, index):
        return self.states[index]

    def add(self, state):
        if state not in self._unique_states:
            self._unique_states.add(state)
            state.end_column = self
            self.states.append(state)
            return True
        return False

    def dump(self, only_completed = False):
        print " [%s] %r" % (self.index, self.token)
        print "=" * 40
        for s in self.states:
            if only_completed and not s.is_completed():
                continue
            print repr(s)
        print "=" * 40
        print

class Node(object):
    def __init__(self, value, children):
        self.value = value
        self.children = children

    def dump(self, level = 0):
        print "  " * level + str(self.value)
        for child in self.children:
            child.dump(level + 1)

# INTERNAL SUBROUTINES FOR EARLEY ALGORITHM
################################################################################

def predict(column, rule):
    for production in rule.productions:
        column.add(
            State(
                rule.name,
                production,
                0,
                column))

def scan(column, state, token):
    if token != column.token:
        return
    column.add(
        State(
            state.name,
            state.production,
            state.dot_index + 1,
            state.start_column))

def complete(column, state):
    if not state.is_completed():
        return
    for prev_state in state.start_column:
        term = prev_state.get_next_term()
        if not isinstance(term, Rule):
            continue
        if term.name == state.name:
            column.add(
                State(
                    prev_state.name,
                    prev_state.production,
                    prev_state.dot_index + 1,
                    prev_state.start_column))

GAMMA_RULE = "GAMMA"

# ENTRY POINT FOR EARLEY ALGORITHM
################################################################################

def parse(starting_rule, text):
    text_with_indexes = enumerate([ None ] + text.lower().split())

    table = [ Column(i, token) for i, token in text_with_indexes ]
    table[0].add(State(GAMMA_RULE, Production(starting_rule), 0, table[0]))

    for i, column in enumerate(table):
        for state in column:
            if state.is_completed():
                complete(column, state)
            else:
                term = state.get_next_term()
                if isinstance(term, Rule):
                    predict(column, term)
                elif i + 1 < len(table):
                    scan(table[i + 1], state, term)
        
        # XXX(sandello): You can uncomment this line to see full dump of
        # the chart table.
        #
        # column.dump(only_completed = False)

    # Find Gamma rule in the last table column or fail otherwise.
    for state in table[-1]:
        if state.name == GAMMA_RULE and state.is_completed():
            return state
    else:
        raise ValueError, "Unable to parse the text."

# AUXILIARY ROUTINES
################################################################################

def build_trees(state):
    return build_trees_helper([], state, len(state.rules) - 1, state.end_column)

def build_trees_helper(children, state, rule_index, end_column):
    if rule_index < 0:
        return [Node(state, children)]
    elif rule_index == 0:
        start_column = state.start_column
    else:
        start_column = None
    
    rule = state.rules[rule_index]
    outputs = []

    for prev_state in end_column:
        if prev_state is state:
            break
        if prev_state is state or not prev_state.is_completed() or prev_state.name != rule.name:
            continue
        if start_column is not None and prev_state.start_column != start_column:
            continue
        for sub_tree in build_trees(prev_state):
            for node in build_trees_helper([sub_tree] + children, state, rule_index - 1, prev_state.start_column):
                outputs.append(node)

    return outputs

def qtree(node):
    # http://yohasebe.com/rsyntaxtree/
    if node.value.name == GAMMA_RULE:
        return qtree(node.children[0])

    sub_qtrees = dict(
        (child.value.name, qtree(child))
        for child in node.children)

    return "[{0} {1}]".format(
        node.value.name,
        " ".join(
            sub_qtrees[term.name] if isinstance(term, Rule) else term
            for term in node.value.production)
        )

################################################################################

def load_grammar(iterable):
    non_terminals = dict()
    starting_rule = None

    def get_term(part):
        if part == part.lower():
            return part
        if part == part.upper():
            if part not in non_terminals:
                non_terminals[part] = Rule(part)
            return non_terminals[part]
        raise RuntimeError, "(unreachable)"

    for n, line in enumerate(iterable):
        parts = line.strip().split()

        for part in parts:
            if part != part.upper() and part != part.lower():
                raise RuntimeError, "Malformed line #{0}: Mixed-case for term '{1}'".format(n + 1, part)

        if len(parts) == 0:
            continue

        if len(parts) == 1:
            if parts[0] not in non_terminals:
                raise RuntimeError, "Malformed line #{0}: Unknown non-terminal '{1}'".format(n + 1, parts[0])
            else:
                starting_rule = parts[0]
                continue

        if parts[1] != "->":
            raise RuntimeError, "Malformed line #{0}: Second part have to be '->'".format(n + 1)

        lhs = get_term(parts[0])
        rhs = map(get_term, parts[2:])

        if not isinstance(lhs, Rule):
            raise RuntimeError, "Malformed line #{0}: Left-hand side have to be a non-terminal".format(n + 1)

        lhs.add(Production(*rhs))

    if starting_rule:
        return non_terminals[starting_rule]
    else:
        return non_terminals["S"]

################################################################################

if __name__ == "__main__":
    # You can specify grammar either by hard-coding it or by loading from file.
    # 
    # (Specifying grammar in code)
    #     SYM  = Rule("SYM", Production("a"))
    #     OP   = Rule("OP",  Production("+"), Production("*"))
    #     EXPR = Rule("EXPR")
    #     EXPR.add(Production(SYM))
    #     EXPR.add(Production(EXPR, OP, EXPR))
    #
    # (Loading grammar from file)
    #     g = load_grammar(open("a.txt"))

    g = load_grammar("""
        N_VOWEL -> elephant
        N_VOWEL -> apple
        
        N_CONS -> hat
        N_CONS -> garden
        N_CONS -> time
        N_CONS -> flight
        N_CONS -> banana
        N_CONS -> boy
        N_CONS -> man
        N_CONS -> telescope
        N_CONS -> sandwich
        N_CONS -> president
        N_CONS -> burger
        N_CONS -> coca-cola
        N_CONS -> pickle
        N_CONS -> floor
        
        N -> N_CONS
        N -> N_VOWEL
        
        N_PL -> flies

        NN -> john
        NN -> mary
        NN -> houston
        NN -> sally

        ADJ_CONS -> giant
        ADJ_CONS -> red
        ADJ_CONS -> perplexed

        ADJ_VOWEL -> every

        ADJ -> ADJ_CONS
        ADJ -> ADJ_VOWEL

        D_CONS -> the
        D_VOWEL -> the
        D_PL -> the
        D_CONS -> a
        D_VOWEL -> an

        ADV_CONS -> very

        V1_VAL12 -> book
        V1_F2_VAL12 -> books
        V2_VAL12 -> booked
        V3_VAL12 -> booked
        
        V1_VAL12 -> eat
        V1_F2_VAL12 -> eats
        V2_VAL12 -> ate
        V3_VAL12 -> eaten
        
        V1_VAL23 -> give
        V1_F2_VAL23 -> gives
        V2_VAL23 -> gave
        V3_VAL23 -> given
       
        V1_VAL2 -> kiss
        V1_F2_VAL2 -> kisses
        V2_VAL2 -> kissed
        V3_VAL2 -> kissed

        V1_VAL1 -> lie
        V1_F2_VAL1 -> lies
        V2_VAL1 -> lay
        V3_VAL1 -> lain

        V1_VAL2 -> see
        V1_F2_VAL2 -> sees
        V2_VAL2 -> saw
        V3_VAL2 -> seen

        V1_VAL1 -> sigh
        V1_F2_VAL1 -> sighs
        V2_VAL1 -> sighed
        V3_VAL1 -> sighed
        
        V1_VAL1 -> sleep
        V1_F2_VAL1 -> sleeps
        V1_VAL1 -> slept
        V1_VAL1 -> slept
        
        V1_VAL1 -> think
        V1_F2_VAL1 -> thinks
        V2_VAL1 -> thought
        V3_VAL1 -> thought

        V1_VAL12 -> want
        V1_F2_VAL12 -> wants
        V2_VAL12 -> wanted
        V3_VAL12 -> wanted

        V1_VAL1 -> walk
        V1_F2_VAL1 -> walks
        V2_VAL1 -> walked
        V3_VAL1 -> walked
      

        V1_VAL1 -> V1_VAL12
        V1_VAL1 -> V1_VAL123
        V1_VAL1 -> V1_VAL13
        
        V1_VAL2 -> V1_VAL12
        V1_VAL2 -> V1_VAL23
        V1_VAL2 -> V1_VAL123

        V1_VAL3 -> V1_VAL13
        V1_VAL3 -> V1_VAL23
        V1_VAL3 -> V1_VAL123

        V1_F2_VAL1 -> V1_F2_VAL12
        V1_F2_VAL1 -> V1_F2_VAL123
        V1_F2_VAL1 -> V1_F2_VAL13
        
        V1_F2_VAL2 -> V1_F2_VAL12
        V1_F2_VAL2 -> V1_F2_VAL23
        V1_F2_VAL2 -> V1_F2_VAL123

        V1_F2_VAL3 -> V1_F2_VAL13
        V1_F2_VAL3 -> V1_F2_VAL23
        V1_F2_VAL3 -> V1_F2_VAL123

        V2_VAL1 -> V2_VAL12
        V2_VAL1 -> V2_VAL123
        V2_VAL1 -> V2_VAL13
        
        V2_VAL2 -> V2_VAL12
        V2_VAL2 -> V2_VAL23
        V2_VAL2 -> V2_VAL123

        V2_VAL3 -> V2_VAL13
        V2_VAL3 -> V2_VAL23
        V2_VAL3 -> V2_VAL123

        V3_VAL1 -> V3_VAL12
        V3_VAL1 -> V3_VAL123
        V3_VAL1 -> V3_VAL13
        
        V3_VAL2 -> V3_VAL12
        V3_VAL2 -> V3_VAL23
        V3_VAL2 -> V3_VAL123

        V3_VAL3 -> V3_VAL13
        V3_VAL3 -> V3_VAL23
        V3_VAL3 -> V3_VAL123


        P -> with
        P -> in
        P -> on
        P -> at
        P -> through

        CONJ -> and
        CONJ -> or
        COMMA -> ,

        PR -> he
        PR -> she
        PR_POS -> his
        PR_POS -> her
        PR_DEF -> that
        PR_DEF -> who
        PR_Q -> that

        QW -> what
        QW -> when
        QW -> where
        QW -> why

        ADV -> ADV_CONS
        ADV -> ADV_VOWEL
        ADV_G_CONS -> ADV_CONS
        ADV_G_VOWEL -> ADV_VOWEL
        ADV_G_CONS -> ADV_G_CONS ADV
        ADV_G_VOWEL -> ADV_G_VOWEL ADV

        ADJ_G_CONS -> ADJ_CONS   
        ADJ_G_VOWEL -> ADJ_VOWEL
        
        ADJ_G_CONS -> ADV_G_CONS ADJ
        ADJ_G_VOWEL -> ADV_G_VOWEL ADJ

        ADJ_G -> ADJ_G_CONS
        ADJ_G -> ADJ_G_VOWEL
        ADJ_G_CONS -> ADJ_G_CONS ADJ_G
        ADJ_G_VOWEL -> ADJ_G_VOWEL ADJ_G

        NP -> NN
        
        NP -> D_VOWEL N_VOWEL
        NP -> D_CONS N_CONS
        
        NP_PL -> D_PL N_PL

        NP -> D_VOWEL ADJ_G_VOWEL N
        NP -> D_CONS ADJ_G_CONS N
        NP -> ADJ_G N
        
        NP_PL -> D_PL ADJ_G N_PL
        NP_PL -> ADJ_G N_PL

        NP -> PR
        NP -> PR
        NP -> PR_POS N
        NP_PL -> PR_POS N_PL
        NP -> NP PP
        NP_PL -> NP_PL PP

        NP_ENUM -> NP COMMA NP
        NP_ENUM -> NP_ENUM COMMA NP
        NP -> NP CONJ NP
        NP -> NP_ENUM CONJ NP

        PP -> P NP
        PP -> P NP_PL

        VP -> V1_VAL1
        VP -> V2_VAL1
        VP_F2 -> V1_F2_VAL1
        VP_F2 -> V2_VAL1

        VP -> V1_VAL2 NP
        VP -> V2_VAL2 NP
        VP_F2 -> V1_F2_VAL2 NP
        VP_F2 -> V2_VAL2 NP

        VP -> V1_VAL3 NP NP
        VP -> V2_VAL3 NP NP
        VP_F2 -> V1_F2_VAL3 NP NP
        VP_F2 -> V2_VAL3 NP NP
  
        VP_ENUM -> VP COMMA VP
        VP_ENUM -> VP_ENUM COMMA VP
        VP -> VP CONJ VP
        VP -> VP_ENUM CONJ VP
        
        VP_F2_ENUM -> VP_F2 COMMA CP
        VP_F2_ENUM -> VP_F2_ENUM COMMA VP_F2
        VP_F2 -> VP_F2 CONJ VP_F2
        VP_F2 -> VP_F2_ENUM CONJ VP_F2

        VP -> VP PP
        VP_F2 -> VP_F2 PP

        VP -> V1_VAL1 PR_Q S
        VP -> V2_VAL1 PR_Q S
        VP_F2 -> V1_F2_VAL1 PR_Q S
        VP_F2 -> V2_VAL1 PR_Q S

        AV -> do
        AV_F2 -> does

        S -> NP VP_F2
        S -> NP_PL VP
        S -> QW AV_F2 NP VP
        S -> QW AV NP_PL VP

        NP -> NP PR_DEF VP_F2
        NP_PL -> NP_PL DEF VP
        NP -> NP PR_DEF NP VP_F2
        NP -> NP PR_DEF NP_PL VP
        NP_PL -> NP PR_DEF NP VP_F2
        NP_PL -> NP_PL PR_DEF NP_PL VP
    """.splitlines())
    
    def parse_and_print(g, s):
      try:
        for tree in build_trees(parse(g, s)):
            print "-" * 80
            print qtree(tree)
            print
            tree.dump()
            print
      except ValueError as e:
        print "%s -> %s"%(s, e)

#1
    parse_and_print(g, "john eat")
    parse_and_print(g, "john eats")
    parse_and_print(g, "his walks with he hat")
    parse_and_print(g, "he walks with his hat")

#2
    parse_and_print(g, "the president sighed")
    parse_and_print(g, "the president thought that a sandwich sighed")
    parse_and_print(g, "the president thought that the president thought that a sandwich sighed")

#3
    parse_and_print(g, "sally ate a sandwich")
    parse_and_print(g, "sally ate a sandwich , a burger and a coca-cola")
    parse_and_print(g, "sally and the president wanted and ate a sandwich")
    parse_and_print(g, "the very very very very perplexed president ate a sandwich")
    parse_and_print(g, "every sandwich with a pickle on the floor wanted a president")
#4
    parse_and_print(g, "an apple lies")
    parse_and_print(g, "a apple lies")
    parse_and_print(g, "a red apple lies")
    parse_and_print(g, "an red apple lies")
    parse_and_print(g, "the red apple lies")

#5
    parse_and_print(g, "the pickle kissed the president that ate the sandwich")
    parse_and_print(g, "the pickle kissed the sandwich that the president ate")
    parse_and_print(g, "the pickle kissed the sandwich that the president thought that Sally ate")
    parse_and_print(g, "what does the president think")
    parse_and_print(g, "what does the president think that sally ate")

#    parse_and_print(g, "book the flight through houston")
#    parse_and_print(g, "john saw the boy with the telescope")
#    parse_and_print(g, "john sleeps")
#    parse_and_print(g, "he gives mary his hat")
#    parse_and_print(g, "an elephant walks in the garden")
#    parse_and_print(g, "a giant man eats a giant apple")
