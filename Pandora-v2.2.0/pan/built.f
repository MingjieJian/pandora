      subroutine BUILT
     $(IX,XJBAR)
C
C     Rudolf Loeser, 2004 Mar 17
C---- Edits selection switches, and sets up compute-authorization
C     switches, for RHO & RBD calculation.
C     !DASH
      save
C     !DASH
      real*8 XJBAR
      integer IX, JBD, JBDNC, JRHO, LUEO, MRA
      logical DUMP, GOOD, JBARBAD
C     !COM
C---- TULLY       as of 2004 Mar 17
      integer     MBD,MRHO
      logical     KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C
      common      /TULLY1/ MBD,MRHO
      common      /TULLY2/ KRHOJ,KRHOW,KRBDS,KRBDJ,KRBDR,KRBDQ
C     Intermediates for TULIP: RHO & RBD calculation.
C     .
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 10),JBD  )
      equivalence (KZQ( 12),JRHO )
      equivalence (KZQ( 40),JBDNC)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external PINYON, MUSTY, MUKI, MESHED, MASHED, HALT, HI, BYE
C
      dimension IX(*)
C
C               XJBAR(N,NT)
      dimension XJBAR(*)
C
      call HI ('BUILT')
C     !BEG
      call MUSTY  (IX, MRA)
      call PINYON (XJBAR, GOOD)
      JBARBAD = .not.GOOD
C
      MBD   = JBD
      MRHO  = JRHO
      KRHOJ = .true.
      KRHOW = .true.
      KRBDS = .true.
      KRBDJ = .true.
      KRBDR = .true.
      KRBDQ = .true.
C
      if(JBARBAD) then
        KRBDJ = .false.
        if(MRA.eq.2) then
          KRBDR = .false.
          KRHOJ = .false.
          KRHOW = .false.
        end if
      end if
C
      if(.not.KRHOJ) then
        MRHO = 0
      end if
      if(.not.KRBDJ) then
        if(.not.KRBDR) then
          MBD = 2
        else
          if(MBD.eq.0) then
            MBD = 1
          end if
        end if
      end if
C
      if(JBDNC.eq.1) then
        KRHOJ = MRHO.eq.1
        KRHOW = MRHO.eq.2
        KRBDS = .false.
        KRBDJ = MBD.eq.0
        KRBDR = MBD.eq.1
        KRBDQ = MBD.eq.2
      end if
C     !EJECT
      call MUKI     (DUMP)
      if(DUMP.or.JBARBAD) then
        call MESHED ('BUILT', 3)
        write (LUEO,100) JBD,JRHO,MRA,JBARBAD,JBDNC,KRHOJ,KRHOW,
     $                   KRBDS,KRBDJ,KRBDR,KRBDQ,MBD,MRHO
  100   format(' ','Controls for TULIP (aka RHO & RBD calculation)'//
     $         ' ','JBD =',I4,5X,'JRHO =',I4,5X,'MRA =',I4,5X,
     $             'JBARBAD =',L4,5X,'JBDNC =',I4//
     $         ' ','KRHOJ =',L4,5X,'KRHOW =',L4,10X,'KRBDS =',L4,5X,
     $             'KRBDJ =',L4,5X,'KRBDR =',L4, 5X,'KRBDQ =',L4//
     $         ' ','MBD =',I4,5X,'MRHO =',I4)
        call MASHED ('BUILT')
      end if
C
      if((MBD.lt.0).or.(MBD.gt.2)) then
        write(MSSLIN(1),101) 'MBD',MBD
  101   format(A,' =',I12,', which is neither 0, 1, nor 2.')
        call HALT   ('BUILT', 1)
      end if
      if((MRHO.lt.0).or.(MRHO.gt.2)) then
        write(MSSLIN(1),101) 'MRHO',MRHO
        call HALT   ('BUILT', 1)
      end if
C     !END
      call BYE ('BUILT')
C
      return
      end
