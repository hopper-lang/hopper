module SignedBy where

open import Data.String
open import Relation.Binary.PropositionalEquality
open import Data.Nat

-- this is easy to write
--- but ignoring for now
postulate  Word512 : Set

{-
signed by val with noncing by identity ==
really needs to be part of a freshly generated challenge /uuid string mixed into the response to
ensure it can be MIM'd and rerouted to unrelated transactions

a related issue is making sure the signeatures at values and on the overall module/transaction unit are
'entangled' together so that manipulations wont validate

for manny applications it'd suffice to just do a signed by of a module/program as a whole
-}
postulate  signed_nonced_by_ : {a : Set} -> a -> Word512 -> String -> Set
postulate  canonicalHash : {a : Set} -> a -> Word512
postulate canonicalNoncedHash : { a : Set} -> a -> Word512 -> Word512

postulate getSignedVal : {a : Set} ->  {nonce : Word512 } { sig : String } {v : a} (sigVal : signed v nonced  nonce by  sig ) -> a
postulate getSignedValEta :  {a : Set}  {v : a} { n : Word512} {i : String} ->  (x : signed v nonced n by i ) -> ( v ≡  getSignedVal x )

{-

-}
