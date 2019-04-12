      subroutine SATRAP
     $(LU,MN1,N,Z,TE,ZT,KZ,NZ,JINC)
C
C     Rudolf Loeser, 2004 Feb 03
C---- Prints for STACK.
C     !DASH
      save
C     !DASH
      real*8 TE, Z, ZT
      integer I, J, JE, JINC, JS, KZ, LU, MN1, N, NZ
C     !DASH
      external  LINER, SHIM, HI, BYE
      intrinsic min
C
      dimension KZ(N,*), ZT(*), TE(*), Z(*)
C
      call HI ('SATRAP')
C     !BEG
      JE = 1
  100 continue
        JS = JE
        JE = min((JS+19),NZ)
        call LINER  (2, LU)
        write (LU,101) (J,J=(JS-JINC),(JE-JINC))
  101   format(' ',4X,'i',12X,'TE',12X,'ZT',13X,'Z',20I4)
        call LINER  (1, LU)
        do 103 I = 1,MN1
          write (LU,102) I,TE(I),ZT(I),Z(I),(KZ(I,J),J=JS,JE)
  102     format(' ',I5,1P3E14.6,20I4)
          call SHIM (I, 5, LU)
  103   continue
      if(JE.lt.NZ) goto 100
C     !END
      call BYE ('SATRAP')
C
      return
      end
