      subroutine DAIT
     $(NO,T,METNAM,KNT,IU,IL)
C
C     Rudolf Loeser, 1974 Mar 22
C---- Prints Statistical Equilibrium methods timings.
C     !DASH
      save
C     !DASH
      real*8 T, ZERO
      integer I, IL, IU, KNT, NO
      character METNAM*15
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LINER, HI, BYE
C
C               T(KNT), METNAM(KNT)
      dimension T(*),   METNAM(*)
C
      call HI ('DAIT')
C     !BEG
      if(NO.gt.0) then
        call LINER (3,NO)
        write (NO,100)
  100   format(' ','Time needed to compute Statistical Equilibrium ',
     $             'equations:')
        call LINER (1,NO)
C
        do 102 I = 1,KNT
          if(T(I).gt.ZERO) then
            write (NO,101) METNAM(I),IU,IL,T(I)
  101       format(' ',A20,' (',I2,'/',I2,')',F7.3,' sec.')
          end if
  102   continue
C
      end if
C     !END
      call BYE ('DAIT')
C
      return
      end
