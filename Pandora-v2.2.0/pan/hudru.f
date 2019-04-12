      subroutine HUDRU
     $(NKA,ZALBK,N,Z,KZXST,ALBK,CQM,NCQ)
C
C     Rudolf Loeser, 1983 Oct 13
C---- Sets defaults for scattering albedo tables.
C     (For Kurucz Opacities).
C     (This is version 2 of HUDRU.)
C     !DASH
      save
C     !DASH
      real*8 ALBK, CQM, ONE, Z, ZALBK, ZERO
      integer KZXST, N, NCQ, NKA
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external HALT, HI, BYE
C
C               Z(N), ZALBK(NKA), ALBK(NKA)
      dimension Z(*), ZALBK(*),   ALBK(*)
C     !EJECT
C
      call HI ('HUDRU')
C     !BEG
      if(NKA.eq.2) then
        if(KZXST.gt.0) then
          if(ZALBK(1).eq.ZZLARGE) then
            ZALBK(1) = Z(1)
          end if
          if( ALBK(1).eq.ZZLARGE) then
            ALBK(1) = ONE
          end if
          if(ZALBK(2).eq.ZZLARGE) then
            ZALBK(2) = Z(N)
          end if
          if( ALBK(2).eq.ZZLARGE) then
            ALBK(2) = ZERO
          end if
        end if
C
      else if(NKA.le.0) then
C
        if((CQM.eq.ZERO).and.(NCQ.le.0)) then
          write (MSSLIN(1),100) CQM,NCQ
  100     format('Scattering Albedo: CQM =',1PE12.4,5X,'NCQ =',I2)
          call HALT ('HUDRU',1)
        end if
      end if
C     !END
      call BYE ('HUDRU')
C
      return
      end
