      subroutine GUPTA
     $(KOMPO,KOMNP,KOMNT,KOMNV,BANDL,BANDU,BANDY,NAB,WAVCO,YWVCO,INWVC,
     $ NCP,WAVES,KWC)
C
C     Rudolf Loeser, 1983 Jun 30
C---- Sets up data for wavelengths at which Composite Line Opacity
C     is specified.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, BANDY, TEN, WAVCO, WAVE, WAVES, YWVCO
      integer I, IB, IN, INWVC, KOMNP, KOMNT, KOMNV, KOMPO, KWC, NAB,
     $        NCP, NW
      logical MEMBER
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external  KURASH, HALT, HI, BYE
      intrinsic max
C
C               WAVCO(NCP), YWVCO(NCP), INWVC(NCP), WAVES(KWC),
      dimension WAVCO(*),   YWVCO(*),   INWVC(*),   WAVES(*),
C
C               BANDY(NAB), BANDL(NAB), BANDU(NAB)
     $          BANDY(*),   BANDL(*),   BANDU(*)
C     !EJECT
C
      call HI ('GUPTA')
C     !BEG
      rewind KOMPO
      read (KOMPO,100) NW,KOMNP,KOMNT,KOMNV
  100 format(4I20)
      KOMNV = max(KOMNV,1)
      if(NW.ne.KWC) then
        write (MSSLIN(1),101) NW,KWC
  101   format('NW =',I12,', KWC =',I12,'; they should be equal.')
        call HALT     ('GUPTA',1)
      end if
C
      read (KOMPO,102) (WAVES(I),I=1,KWC)
  102 format(4E20.12)
C
      IN = 0
      do 104 I = 1,KWC
        WAVE = TEN*WAVES(I)
        call KURASH   (NAB,BANDL,BANDU,WAVE,MEMBER,IB)
        if(MEMBER) then
          IN = IN+1
          if(IN.gt.NCP) then
            write (MSSLIN(1),103) IN,NCP
  103       format('IN =',I12,', NCP =',I12,', which is not allowed.')
            call HALT ('GUPTA',1)
          end if
          WAVCO(IN) = WAVE
          YWVCO(IN) = BANDY(IB)
          INWVC(IN) = I
        end if
  104 continue
C
      if(IN.ne.NCP) then
        write (MSSLIN(1),105) IN,NCP
  105   format('IN =',I12,', NCP =',I12,'; they should be equal.')
        call HALT     ('GUPTA',1)
      end if
C     !END
      call BYE ('GUPTA')
C
      return
      end
