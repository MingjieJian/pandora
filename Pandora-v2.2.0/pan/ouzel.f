      subroutine OUZEL
     $(N,A,LAB,KILROY,CALLER)
C
C     Rudolf Loeser, 2004 May 21
C---- Checks number densities for HOOPOE.
C     (This is version 2 of OUZEL.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer KNT, LUEO, N
      logical KILROY, OK
      character CALLER*(*), LAB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PLUSD, MUSHED, VECOUT, HI, BYE
C
C               A(N)
      dimension A(*)
C
      call HI ('OUZEL')
C     !BEG
      call PLUSD    (A, 1, N, KNT)
      OK = KNT.eq.N
C
      if(.not.OK) then
        call MUSHED (CALLER, 3, KILROY)
        write (LUEO,100)
  100   format(' ','There are values .le.0 in the following number ',
     $             'density table:')
        call VECOUT (LUEO, A, N, LAB)
      end if
C     !END
      call BYE ('OUZEL')
C
      return
      end
