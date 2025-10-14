module Rectify exposing (rectify)

import Expr exposing (Expr(..), Id, foldrExpr, recrExpr)
import Multiset exposing (Multiset)
import Set exposing (Set)
import State exposing (State, lift, lift2, lift3, new)
import String exposing (fromInt)
import Utils exposing (until)


freeAndBoundVars : Expr -> ( Set Id, Multiset Id )
freeAndBoundVars =
    foldrExpr
        (\id -> ( Set.singleton id, Multiset.empty ))
        (\id ( free, bound ) ->
            ( Set.remove id free, Multiset.insert id bound )
        )
        (\( free1, bound1 ) ( free2, bound2 ) ->
            ( Set.union free1 free2, Multiset.union bound1 bound2 )
        )
        ( Set.empty, Multiset.empty )
        ( Set.empty, Multiset.empty )
        identity
        ( Set.empty, Multiset.empty )
        identity
        identity
        (\( free1, bound1 ) ( free2, bound2 ) ( free3, bound3 ) ->
            ( Set.union free1 (Set.union free2 free3), Multiset.union bound1 (Multiset.union bound2 bound3) )
        )


renameVar : Id -> Id -> Expr -> Expr
renameVar oldId newId =
    let
        ifIsOldId id a b =
            if id == oldId then
                a

            else
                b
    in
    recrExpr
        (\id -> Var (ifIsOldId id newId id))
        (\id e1 rec1 -> Abs id (ifIsOldId id e1 rec1))
        (\_ rec1 _ rec2 -> App rec1 rec2)
        ConstTrue
        ConstFalse
        (\_ -> IsZero)
        ConstZero
        (\_ -> Succ)
        (\_ -> Pred)
        (\_ rec1 _ rec2 _ rec3 -> If rec1 rec2 rec3)


rectifyHelper : Expr -> Set Id -> State Expr (Multiset Id)
rectifyHelper =
    foldrExpr
        (\id _ -> new (Var id))
        (\id fRec freeVars boundVars ->
            if Multiset.count id boundVars == 1 && not (Set.member id freeVars) then
                lift (Abs id) (fRec freeVars) boundVars

            else
                let
                    newId =
                        freshId id freeVars boundVars

                    newFreeVars =
                        Set.insert newId freeVars

                    newBvs =
                        Multiset.remove id boundVars
                            |> Multiset.insert newId
                in
                lift
                    (\rec1 -> Abs newId (renameVar id newId rec1))
                    (fRec newFreeVars)
                    newBvs
        )
        (\fRec1 fRec2 freeVars -> lift2 App (fRec1 freeVars) (fRec2 freeVars))
        (\_ _ -> ( ConstTrue, Multiset.empty ))
        (\_ _ -> ( ConstFalse, Multiset.empty ))
        (\fRec freeVars -> lift IsZero (fRec freeVars))
        (\_ _ -> ( ConstZero, Multiset.empty ))
        (\fRec freeVars -> lift Succ (fRec freeVars))
        (\fRec freeVars -> lift Pred (fRec freeVars))
        (\fRec1 fRec2 fRec3 freeVars ->
            lift3 If (fRec1 freeVars) (fRec2 freeVars) (fRec3 freeVars)
        )


freshId : Id -> Set Id -> Multiset Id -> Id
freshId id freeVars boundVars =
    let
        isAvailable nId =
            let
                idCandidate =
                    id ++ fromInt nId
            in
            not (Set.member idCandidate freeVars)
                && not (Multiset.member idCandidate boundVars)

        freeN =
            until isAvailable ((+) 1) 1
    in
    id ++ fromInt freeN


rectify : Expr -> Expr
rectify expr =
    let
        ( freeVars, boundVars ) =
            freeAndBoundVars expr
    in
    Tuple.first <|
        rectifyHelper expr freeVars boundVars
