      subroutine ELK
     $(XJNU,TE,WAVE,A,KOOL,X,RM,RD,DUMP)
C
C     Rudolf Loeser, 1984 Aug 21
C---- Gets integration data at a specific depth and frequency,
C     for AMON.
C     (This is version 2 of ELK.)
C     !DASH
      save
C     !DASH
      real*8 A, CON7, DIV, HNUKT, ONE, R, RD, RM, SEF, TE, WAVE, X,
     $       XJNU, ZERO
      integer LUEO
      logical DUMP, KOOL
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ANGIE, PROD, RIGEL, HI, BYE
C     !EJECT
C
      call HI ('ELK')
C     !BEG
      call ANGIE   (WAVE,X)
      if(A.eq.ZERO) then
        RM = ZERO
        RD = ZERO
      else
        if(KOOL) then
          DIV = ONE
        else
          DIV = X
        end if
        call PROD  (TE,X,1,HNUKT,SEF)
        call RIGEL (7,CON7)
        R  = A/DIV
        RM = R*XJNU
        RD = R*SEF*(CON7*(X**3)+XJNU)
      end if
C
      if(DUMP) then
        write (LUEO,100) XJNU,TE,WAVE,A,X,DIV,R,SEF,HNUKT
  100   format(' ','ELK',10X,1P4E20.12/
     $         ' ',      13X,  5E20.12)
      end if
C     !END
      call BYE ('ELK')
C
      return
      end
