      subroutine HIT
     $(LU,N,REFLM,INDEX,ZZERO,PO,PREF,P5,TAU5,TAUKIN,ZP,ZO,Z)
C
C     Rudolf Loeser, 1994 May 17
C---- Prints, for DEAD.
C     (This is version 4 of HIT.)
C     !DASH
      save
C     !DASH
      real*8 ONE, P5, PO, PREF, REFLM, TAU5, TAUKIN, Z, ZO, ZP, ZZERO
      integer I, INDEX, LU, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LINER, SHIM, HI, BYE
C
C               PO(N), PREF(N), P5(N), TAU5(N), TAUKIN(N), ZP(N), Z(N),
      dimension PO(*), PREF(*), P5(*), TAU5(*), TAUKIN(*), ZP(*), Z(*),
C
C               ZO(N)
     $          ZO(*)
C     !EJECT
C
      call HI ('HIT')
C     !BEG
      if(LU.gt.0) then
C
        call LINER  (2,LU)
        write (LU,100) REFLM,ZZERO,INDEX,(INDEX+1)
  100   format(' ','Recalculation of Z scale from TAUKIN( ',
     $             1PE14.8,' )'/
     $         ' ','TAU( 5000 ) = 1 occurs at Z(raw) = ',E14.8,
     $             ', between depth points #',I4,' and #',I4///
     $         ' ',11X,'previous',12X,'new',57X,'raw',
     $             7X,'previous',12X,'new'/
     $         ' ',15X,'PREF',11X,'PREF',10X,'P5000',10X,'T5000',
     $             9X,'TAUKIN',14X,'Z',14X,'Z',14X,'Z')
        call LINER  (1,LU)
C
        do 103 I = 1,N
          write (LU,101) I,PO(I),PREF(I),P5(I),TAU5(I),TAUKIN(I),ZP(I),
     $                   ZO(I),Z(I)
  101     format(' ',I4,1P8E15.7)
          if(I.eq.INDEX) then
            write (LU,102) ONE,ZZERO
  102       format(' ',49X,1PE15.7,15X,E15.7)
          end if
          call SHIM (I,5,LU)
  103   continue
C
      end if
C     !END
      call BYE ('HIT')
C
      return
      end
