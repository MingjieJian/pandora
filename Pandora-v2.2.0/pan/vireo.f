      subroutine VIREO
     $(VMIN,E,NV,V,VL)
C
C     Rudolf Loeser, 1986 Jul 06
C---- Sets up V and VL tables, for FINCH.
C     !DASH
      save
C     !DASH
      real*8 BASE, E, ONE, RE, T, TWO, V, VL, VMIN, XN, XNV, ZERO
      integer I, NV
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
C               V(NV), VL(NV)
      dimension V(*),  VL(*)
C
      call HI ('VIREO')
C     !BEG
      if((VMIN.le.ZERO).or.(NV.le.0)) then
        write (MSSLIN(1),100) NV,VMIN
  100   format('NV =',I12,', VMIN =',1PE12.4,'; bad values.')
        call HALT ('VIREO',1)
      end if
C
      if(NV.eq.1) then
        V(1)  = VMIN
        VL(1) = log(V(1))
      else
C
        XNV  = NV-1
        RE   = sqrt(TWO*E-ONE)
        BASE = RE/VMIN
        do 101 I = 1,NV
          XN = I-1
          T  = BASE**(XN/XNV)
          V(I)  = VMIN*T
          VL(I) = log(V(I))
  101   continue
      end if
C     !END
      call BYE ('VIREO')
C
      return
      end
