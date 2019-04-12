      subroutine PRATE
     $(A,N,AS,NS,KODE)
C
C     Rudolf Loeser, 1976 Mar 04
C---- Establishes and checks a subset.
C
C     AS (length NS) should be a subset of A (length N). If the contents
C     of AS .eq. 0, then set AS = A. If the contents of AS .ne. 0, then
C     check whether the elements of AS are in the same order as those of
C     A, and that every element of AS has a counterpart in A.
C
C     (ASSUME that all elements of A are distinct.)
C     (This is version 2 of PRATE.)
C     !DASH
      save
C     !DASH
      real*8 A, AS, ZERO
      integer KODE, N, NS
      logical KA
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NAUGHTD, SUBSSD, MOVE1, HI, BYE
C
C               A(N), AS(N)
      dimension A(*), AS(*)
C
      call HI ('PRATE')
C     !BEG
      KODE = 0
      if(NS.gt.0) then
        call NAUGHTD  (AS, 1, NS, KA)
        if(.not.KA) then
          call SUBSSD (A, N, AS, NS, ZERO, KODE)
        else
          NS = N
          call MOVE1  (A, N, AS)
          KODE = 1
        end if
      else
        NS = N
        call MOVE1    (A, N, AS)
        KODE = 1
      end if
C     !END
      call BYE ('PRATE')
C
      return
      end
