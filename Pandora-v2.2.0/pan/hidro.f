      subroutine HIDRO
     $(ABD,QELSM,QNAME)
C
C     Rudolf Loeser, 1969 Jul 29
C---- Finds the default value of ABD.
C     !DASH
      save
C     !DASH
      real*8 ABD, ZERO, dummy
      integer JSTCN, NOION, jummy
      character ABREV*3, BLANK*1, QELSM*8, QNAME*8
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 35),JSTCN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external FRANK, HALT, HI, BYE
C     !EJECT
C
      call HI ('HIDRO')
C     !BEG
      if((NOION.le.0).and.(JSTCN.le.0)) then
        if(ABD.le.ZERO) then
          if(QELSM(1:2).ne.'ZZ') then
            ABREV = QELSM(1:2)//BLANK
            call FRANK (ABREV,0,ABD,dummy,dummy,dummy,jummy)
          end if
        end if
        if(ABD.le.ZERO) then
          write (MSSLIN(1),100) ABD,QELSM,QNAME
  100     format('Abundance =',1PE12.4,5X,A2,5X,A8)
          call HALT    ('HIDRO',1)
        end if
      end if
C     !END
      call BYE ('HIDRO')
C
      return
      end
