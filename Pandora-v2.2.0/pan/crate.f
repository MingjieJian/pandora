      subroutine CRATE
     $(VXI,N,CVXS)
C
C     Rudolf Loeser, 1990 Apr 26
C---- Forces VXI=0.
C     (This is version 2 of CRATE.)
C     !DASH
      save
C     !DASH
      real*8 CVXS, VXI, ZERO
      integer LUEO, N
      logical VZERO
      character LINE*127
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external NAUGHTD, LINER, LABFIL, MESHED, VECOUT, ZERO1, HI, BYE
C
C               VXI(N)
      dimension VXI(*)
C
      call HI ('CRATE')
C     !BEG
      call NAUGHTD   (VXI, 1, N, VZERO)
C
      if(.not.VZERO) then
        call MESHED ('CRATE', 3)
        call LABFIL ('VXI (= VXS-zero)', LINE)
        call VECOUT (LUEO, VXI, N, LINE)
        call LINER  (1, LUEO)
        write (LUEO,100)
  100   format(' ','Error: "Source Function Expansion Velocity", not ',
     $             '= 0 does not make sense when option EXPAND = off.'//
     $         ' ','VXI (= VXS-zero) will be set =0.')
C
        call ZERO1  (VXI, N)
        CVXS = ZERO
      end if
C     !END
      call BYE ('CRATE')
C
      return
      end
