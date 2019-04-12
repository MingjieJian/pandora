      subroutine CYRUS
     $(NAB,KOMPO,KWC,BANDL,BANDU,WAVES,NCP)
C
C     Rudolf Loeser, 1983 Jun 30
C---- Determines NCP, the number of composite line opacity wavelengths
C     to be treated.
C     !DASH
      save
C     !DASH
      real*8 AWAVE, BANDL, BANDU, TEN, WAVES
      integer I, KOMPO, KWC, LWC, NAB, NCP, jummy
      logical MEMBER
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
      external KURASH, HALT, HI, BYE
C
C               BANDL(NAB), BANDU(NAB), WAVES(KWC)
      dimension BANDL(*),   BANDU(*),   WAVES(*)
C     !EJECT
C
      call HI ('CYRUS')
C     !BEG
      NCP = 0
      if(NAB.gt.0) then
        rewind KOMPO
C
        read (KOMPO,100) LWC
  100   format(4I20)
        if(KWC.ne.LWC) then
          write (MSSLIN(1),101) KWC,LWC
  101     format('Data mixup: KWC (given) =',I12,', LWC (found) =',I12)
          call HALT   ('CYRUS',1)
        end if
C
        read (KOMPO,102) (WAVES(I),I=1,KWC)
  102   format(4E20.12)
C
        do 103 I = 1,KWC
          AWAVE = TEN*WAVES(I)
          call KURASH (NAB,BANDL,BANDU,AWAVE,MEMBER,jummy)
          if(MEMBER) then
            NCP = NCP+1
          end if
  103   continue
C
      end if
C     !END
      call BYE ('CYRUS')
C
      return
      end
