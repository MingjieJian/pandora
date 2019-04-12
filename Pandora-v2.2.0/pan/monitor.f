      subroutine MONITOR
     $(N,Z,KZERO)
C
C     Rudolf Loeser, 1981 Jul 29
C---- Finds KZERO, the NH-adjustment index.
C     Upon return,
C     if KZERO .gt. 0, then KZERO= I, such that Z(I)=0.
C     if KZERO .lt. 0, then KZERO=-J, such that Z(J) .lt. 0 .lt. Z(J+1).
C     (This is version 4 of MONITOR.)
C     !DASH
      save
C     !DASH
      real*8 Z, Z1, ZERO, ZI, ZN
      integer I, KODE, KZERO, LUEO, N
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
C     !DASH
      external MESHED, ABORT, PRIVET, HI, BYE
C
C               Z(N)
      dimension Z(*)
C     !EJECT
C
      call HI ('MONITOR')
C     !BEG
      KODE = 1
C
      Z1 = Z(1)
      ZN = Z(N)
      if(Z1.eq.ZERO) then
        KZERO=1
      else if(ZN.eq.ZERO) then
        KZERO=N
      else if((Z1.gt.ZERO).or.((Z1.lt.ZERO).and.(ZN.lt.ZERO))) then
        KODE=0
      else
C
        do 100 I = 2,(N-1)
          ZI = Z(I)
          if(ZI.eq.ZERO) then
            KZERO = I
            goto 101
          else if(ZI.gt.ZERO) then
            KZERO = -(I-1)
            goto 101
          end if
  100   continue
C
        KODE = 0
  101   continue
      end if
C
      if(KODE.eq.0) then
        call MESHED ('MONITOR',1)
        write (LUEO,102)
  102   format(' ','The NH-adjustment procedure cannot find Z=0 ',
     $             'within the current range of Z values.')
        call PRIVET (LUEO,Z,N)
        call ABORT
      end if
C     !END
      call BYE ('MONITOR')
C
      return
      end
