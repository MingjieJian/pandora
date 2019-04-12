      subroutine WARNPOS
     $(A,N,KODE,TIT,CALLER,PRNT,BAD)
C
C     Rudolf Loeser, 2005 Mar 15
C---- Checks whether any value(s) of A (length N) are BAD.
C     If KODE = 1, then BAD means .le. 0;
C        KODE = 2, then BAD means .lt. 0.
C
C     If BAD = .true. and PRNT = .true., a warning is printed.
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer I, KNT, KODE, LUEO, N
      logical BAD, PRNT
      character CALLER*(*), TIT*(*)
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
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, VECOUT, LINER, HALT, MASHED, HI, BYE
C
C               A(N)
      dimension A(*)
C
      call HI ('WARNPOS')
C     !BEG
      if((KODE.lt.1).or.(KODE.gt.2)) then
        write (MSSLIN(1),100) KODE
  100   format('KODE =',I12,', which is not 1 or 2.')
        call HALT   ('WARNPOS', 1)
      end if
C
      KNT = 0
      do 101 I = 1,N
        if(KODE.eq.1) then
          if(A(I).le.ZERO) then
            KNT = KNT+1
          end if
        else
          if(A(I).lt.ZERO) then
            KNT = KNT+1
          end if
        end if
  101 continue
      BAD = KNT.gt.0
C
      if(BAD.and.PRNT) then
        call MESHED ('WARNPOS', 3)
        call VECOUT (LUEO, A, N, TIT)
        call LINER  (1, LUEO)
        write (LUEO,102) KNT,CALLER,KODE
  102   format(' ','This table has',I8,' BAD values.'/
     $         ' ','WARNPOS called from: ',A,'; with KODE =',I2,'.')
        call MASHED ('WARNPOS')
      end if
C     !END
      call BYE ('WARNPOS')
C
      return
      end
