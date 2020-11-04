open ReleaseTypes

module Ints      : NiceRing   = Numbers.Ints
module Integers  : NiceRing   = Numbers.Integers
module Floats    : NiceField  = Numbers.Floats
module Root23    : Root23Type = Numbers.Root23
module Rationals : NiceField  = Numbers.Rationals
module FoFInts   : NiceField  = Numbers.FieldOfFractions(Numbers.Ints)
module FoFFloats : NiceField  = Numbers.FieldOfFractions(Numbers.Floats)
module Rot15     : Rot15Type  = Numbers.Rot15
module Reals     : RealsType  = Numbers.Reals

module QuotientUtils     :  QUType = Numbers.QuotientUtils
module GroupUtils        :  GUType = Numbers.GroupUtils
module RingUtils         :  RUType = Numbers.RingUtils
module FieldUtils        :  FUType = Numbers.FieldUtils
module OrderedRingUtils  : ORUType = Numbers.OrderedRingUtils
module OrderedFieldUtils : OFUType = Numbers.OrderedFieldUtils

(******************************************************************************)

module Geom : GeometryType = Geometry.Make

(******************************************************************************)

module Game : GameType = Game.Make

(*
** vim: ts=2 sw=2 ai et
*)
