      subroutine PLAIN
     $(F,FL,N,LABEL)
C
C     Rudolf Loeser, 1979 Oct 30
C---- Gets logs, for "standard" second-order integration.
C     (This is version 2 of PLAIN.)
C     !DASH
      save
C     !DASH
      real*8 F, FL, ZERO
      integer I, LUEO, N
      character LABEL*(*)
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
      external MESHED, DPRIVE, ABORT, HI, BYE
C
C               F(N), FL(N)
      dimension F(*), FL(*)
C
      call HI ('PLAIN')
C     !BEG
      do 101 I = 1,N
C
        if(F(I).le.ZERO) then
          call MESHED ('PLAIN', 1)
          write (LUEO,100) LABEL
  100     format(' ','Cannot compute logarithms for:'/
     $           ' ',A)
          call DPRIVE (LUEO, F, N)
          call ABORT
        end if
C
        FL(I) = log(F(I))
  101 continue
C     !END
      call BYE ('PLAIN')
C
      return
      end
