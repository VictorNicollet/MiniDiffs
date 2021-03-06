`MiniDiff` is a small drop-in module for computing and applying string diffs.

## Introduction

Computing a diff between `old_string` and `new_string` is done as: 

    let the_diff = MiniDiff.diff old_string new_string

Applying the diff to `old_string` is done as : 

    let applied = MiniDiff.apply the_diff old_string
     
And, of course, the result is equal to `new_string`:

    let _ = assert (applied = new_string)

## Diff format

The `diff` type a `changes` field that is a list of instructions that are followed, in turn, to reconstruct the new string based on the old string. These instructions are:

 - `FromOld (start,length)` which reads `length` characters from the old string starting at offset `start`. 
 - `FromNew (length)` which reads `length` characters from the `new_text` field of the `diff` value.

For instance, to turn the string `abcdef` into the string `abXYefZW`, the instructions could be: 

    { new_text = "XYZW" ;
      changes  = [ FromOld (0,2) ;
                   FromNew 2 ;
                   FromOld (0,2) ;
                   FromNew 2 ]
    }

    // JSON:

    ["XYZW",[0,2],2,[0,2],2]

Note that the second `FromOld (0,2)` has a start offset of zero. This is because the offset is computed based on the position of the character being written, rather than the start of the string: the character at position 4 in the new string is also at position 4 in the old string, so the offset is zero. This helps keep the offset short even if the strings are long.

In practice, however, the algorithm would notice that the diff is actually larger than the string, so the generated diff would probably be:

    { new_text = "abXYefZW" ;
      changes  = [ FromNew 8 ]
    }

    // JSON:
    ["abXYefZW",8]

I have also provided a JavaScript reader for applying diffs in JavaScript, if necessary (I doubt this will be useful on the client, but it can be helpful on the server, whether in Node or CouchDB).

## Purpose

This diff algorithm was designed to serve in a CouchDB-based wiki application. As such, it was created with the following objectives in mind:

 - The code should remain short
 - It should work fast on a small- to medium-sized human-written text (1,000 to 50,000 characters).
 - The resulting diff should be nearly optimal in terms of JSON storage size.

In particular, the diff algorithm will detect and handle text movement (instead of counting that movement as a delete-and-insert operation).

On the other hand, these features are specifically not handled: 

 - Reversibility: if you have a diff to get from `A` to `B`, you cannot turn that diff into another that gets from `B` to `A`
 - Composition: there are no simple ways to turn create a diff that is equivalent to applying two other diffs in succession
 - Sane handling of million-character texts or non-text data
 - Treating newlines or whitespace differently from other characters

Also, this algorithm is designed for storage rather than human-friendly display. If you wish to display the changes between two versions, you might want to look for a different algorithm that takes into account words and punctuation.

## How it works

The algorithm works on a statistical basis by associating identical two-character sequences between the source and destination. For example, the string `The quick brown fox` would be associated to `Brown foxes are quick` as follows

    T  h  e     q  u  i  c  k     b  r  o  w  n     f  o  x
     00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 

    B  r  o  w  n     f  o  x  e  s     a  r  e     q  u  i  c  k
        11 12 13 14 15 16 17                   02 03 04 05 06 07
  
    { new_text = "Besar" ;
      changes  = [ FromNew 1 ;
                   FromOld (10,8) ;
                   FromNew 5 ;
                   FromOld (-12,7) ]
    }

    // JSON:
    ["Bes ar",1,[10,8],5,[-12,7]]

By using character pairs, it is very likely that some pairs will simply not be present, which readily identifies locations where a cut will necessairly happen (whether a move, deletion or insertion) and thus lets the algorithm concentrate independently on the pieces in-between such missing character pairs. Those pieces will also likely contain pairs that are unique in the source text, which is a very fast (actually, constant-time) way to identify where a given piece of text might be coming from.

The algorithm uses these hints to identify reused pieces (which will become `FromOld` instructions) and missing pieces (which will become `FromNew` instructions). In some situations, there will be no unique pairs, which will cause the algorithm to guess a position. This behavior is not optimal, but is fairly infrequent on typical medium-sized human text, so correctly handling it was not part of the algorithm objectives (but you are free to try).

A final pass eliminates sequences that are so small that including them in the `new_text` would use less memory than issuing a `FromOld` instruction. Right now, those are sequences of fewer than 8 characters (so the length parameter of a `FromOld` instruction generated by the algorithm will always be greater than 8). 

Right now, the algorithm itself is almost O(m+n) in the sizes of the two input strings (handling sources as lists instead of hash tables can lead to a performance loss for large texts that contain very frequent characters pairs, but hash tables are overall slower than lists in the average case where the list has only one element, so hash tables are not used.