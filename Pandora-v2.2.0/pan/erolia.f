      subroutine EROLIA
     $(Y,KNT,LABEL,KILROY,CALLER)
C
C     Rudolf Loeser, 1985 Feb 14
C---- Edits Source Function method selection parameters
C     for consistency, and
C     summarizes a Source Function method control parameter table.
C     !DASH
      save
C     !DASH
      real*8 ONE, THREE, TWO, Y, YI, ZERO
      integer I, KNT, KOUNT, KOUNTF, KOUNTS, MSFGR, MSFQM, MSFQR, MSFRT
      logical KILROY
      character CALLER*(*), LABEL*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(16),MSFQR)
      equivalence (LEST(18),MSFQM)
      equivalence (LEST(20),MSFRT)
      equivalence (LEST(21),MSFGR)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external HALT, RUBY, HAIDA, AIMERIC, HI, BYE
C
C               Y(KNT)
      dimension Y(*)
C     !EJECT
C
      call HI ('EROLIA')
C     !BEG
      if(KNT.gt.0) then
        KOUNT  = 0
        KOUNTS = 0
        KOUNTF = 0
C
        do 101 I = 1,KNT
          YI = Y(I)
          call RUBY   (YI, KOUNT, KOUNTS, KOUNTF)
C
          if(YI.eq.-THREE) then
            MSFGR = MSFGR+1
C
          else if(YI.eq.-TWO) then
            MSFQM = MSFQM+1
C
          else if(YI.eq.-ONE) then
            MSFRT = MSFRT+1
C
          else if((YI.ge.ZERO).and.(YI.le.ONE)) then
            MSFQR = MSFQR+1
C
          else
            write (MSSLIN(1),100) I,YI
  100       format('Y(',I6,') =',1PE24.16,' does not make sense.')
            call HALT ('EROLIA', 1)
          end if
C
          Y(I) = YI
  101   continue
C
        call HAIDA    (KOUNT, KOUNTS, KOUNTF, LABEL, KILROY, CALLER)
      end if
C
      call AIMERIC    (KILROY, CALLER, LABEL, KNT)
C     !END
      call BYE ('EROLIA')
C
      return
      end
