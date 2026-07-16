# print

    Code
      fr1
    Output
      # A forest: 6 nodes and 1 feature
      # Trees:    
      #   key1 [2]
      #   \-key2 [4]
        .        value
        <node>   <int>
      1 <key1> a     3
      2 <key1> b     7

---

    Code
      fr
    Output
      # A forest: 12 nodes and 1 feature
      # Trees:    
      #   key1 [2]
      #   \-key2 [4]
      #   key3 [2]
      #   \-key2 [4]
        .        value
        <node>   <int>
      1 <key1> a     3
      2 <key1> b     7
      3 <key3> a     3
      4 <key3> b     7

---

    Code
      fr_ungrouped
    Output
      # A forest: 3 nodes and 1 feature
      # Trees:    
      #   key1 [3]
        .        value
        <node>   <int>
      1 <key1> a     1
      2 <key1> b     2
      3 <key1> c     3

# print with groups and sibling branches

    Code
      fr1
    Output
      # A forest: 12 nodes and 1 feature
      # Groups:   key1 [2]
      # Trees:    
      #   key2_1 [4]
      #   \-key3_1 [8]
        key1  .          value
        <chr> <node>     <int>
      1 a     <key2_1> a     3
      2 a     <key2_1> b     7
      3 b     <key2_1> a    11
      4 b     <key2_1> b    15

---

    Code
      fr_sum
    Output
      # A forest: 26 nodes and 1 feature
      # Trees:    
      #   key1 [2]
      #   +-key2_1 [4]
      #   | \-key3_1 [8]
      #   \-key2_2 [4]
      #     \-key3_2 [8]
        .        value
        <node>   <int>
      1 <key1> a    20
      2 <key1> b    52

