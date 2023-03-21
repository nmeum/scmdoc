{-# LANGUAGE OverloadedStrings #-}

module Record where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text ()
import SchemeDoc.Format.Record (expand)
import Util

------------------------------------------------------------------------

record :: TestTree
record =
    testGroup
        "Tests for Scheme record syntax macro expansion"
        [ testCase "Expand macro w/o comments and single field" $ do
            {- FOURMOLU_DISABLE -}
            let (Right s) = parse "                  \
                \ (define-record-type                \
                \     my-type                        \
                \     (my-cons my-field)             \
                \     my-pred                        \
                \     (my-field getter))"

            let (Right exp) = parse "                \
                \ (begin                             \
                \     (define (my-cons my-field) _)  \
                \     (define (my-pred obj) _)       \
                \     (begin                         \
                \         my-field                   \
                \         (define (getter my-type) _)))"
            {- FOURMOLU_ENABLE -}

            assertEqual "" (Just $ head exp) (expand $ head s)

        , testCase "Expand macro with comments and multiple fields" $ do
            {- FOURMOLU_DISABLE -}
            let (Right s) = parse "                               \
                \ (define-record-type <pare>                      \
                \     ;;> Documentation for my constructor.     \n\
                \     (cons x y)                                  \
                \     ;;> Documentation for my predicate.       \n\
                \     pare?                                       \
                \     (x                                          \
                \         ;;> My getter                         \n\
                \         kar                                     \
                \         ;;> My setter                         \n\
                \         set-kar!)                               \
                \     (y kdr))"

            let (Right exp) = parse "                             \
                \ (begin                                          \
                \     ;;> Documentation for my constructor.     \n\
                \     (define (cons x y) _)                       \
                \     ;;> Documentation for my predicate.       \n\
                \     (define (pare? obj)    _)                   \
                \     (begin                                      \
                \         x                                       \
                \         ;;> My getter                         \n\
                \         (define (kar <pare>) _)                 \
                \         ;;> My setter                         \n\
                \         (define (set-kar! <pare> new-value) _)) \
                \     (begin                                      \
                \         y                                       \
                \         (define (kdr <pare>) _)))"
            {- FOURMOLU_ENABLE -}

            assertEqual "" (Just $ head exp) (expand $ head s)
        ]
