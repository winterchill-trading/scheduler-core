(** Quickcheck generators shared by the property tests. *)

open! Core
open Scheduler_core

(** Dates from 2024 to 2031. *)
val date : Date.t Quickcheck.Generator.t

val frequency : Frequency.t Quickcheck.Generator.t

(** Valid recurrences, some bounded by an end date or a maximum count. *)
val recurrence : Recurrence.t Quickcheck.Generator.t
