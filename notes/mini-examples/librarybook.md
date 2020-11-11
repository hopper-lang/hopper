
 not actually haskell, but lets pretend
```haskell
-- name spacing is GOOOD
module NAME where


data ResourceCondition = LikeNew | ExpectedAging |  Damage String


-- NB : might want to have interfaces for "a thing we can use to Sign with thats part of our
--  trust tree of valid identities that get tied to keys.... which means signatures have TIMES on them"
type alias  OrgIdentity = Identity

data Maybe a = Just a | Nothing

--- when we define a book et al, libOrg is the holding library entity
--- resource IS the UUID of the book
protocol LibraryResource ( libOrg : OrgIdentity)  ( resource : ResourceUUID) where{

 -- states  = OnLoanLocal  | OnLoanInter | Reserved | Shelved | Returned
   -- | ExpiredOnLoan
  --- we kinda want each state to have some local information / private fields
  -- states are all about context, but its nice to have stuff not be history dependent
  states where {
      OnLoanLocal{
      heldBy :  Identity
      ; dueBy : TAI.Time
      ; checkoutRequest : SignedBy RequestID heldBy
      ;checkedOutVia : DelegatedFrom Identity libOrg
      ;CheckoutAccepted : SignedBy RequestID (checkedOutVia.delagatee)
      ;conditionAtCheckout : Maybe (ResourceCondition) }  ;

      ---- signed by heldbyVia requesting org is a natural example of a dependent type/record!
      OnLoanInter{heldbyVia = OrgIdentity ; interRequestId = SignedBy RequestId heldbyVia  ; } ;
      Reserved ;
      Shelved ;
      Returned ;
      ExpiredOnLoan ; ; ;
  }


 messages where
  RenewPropose( idRep : Type ; requester : idRep ;  resquestSig : SignedBy RequestID requester  )
   at OnLoanLocal RenewPropose()

  }

protocol LibraryBorrowing (org : Identity) (borrower : Identity ) (resource : Book )



```
