      subroutine AKALON
     $(KILROY,CALLER,N,RHW,LAB,RHWO,LABO)
C
C     Rudolf Loeser, 2002 Mar 12
C---- Salvages obsolete RHO-weights.
C     (This is version 2 of AKALON.)
C     !DASH
      save
C     !DASH
      real*8 ONE, RHW, RHWO, ZERO
      integer I, KEQ, LUEO, N
      logical KILROY, ZRHWO
      character CALLER*(*), LAB*(*), LABO*(*)
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external RANGED, NAUGHTD, MUSHED, HI, BYE
C
C               RHW(N), RHWO(N)
      dimension RHW(*), RHWO(*)
C
      call HI ('AKALON')
C     !BEG
      call RANGED     (RHW, 1, N, ZERO, ONE, KEQ)
      if(KEQ.eq.N) then
C
        call NAUGHTD  (RHWO, 1, N, ZRHWO)
        if(.not.ZRHWO) then
          do 100 I = 1,N
            RHW(I) = ONE-RHWO(I)
  100     continue
C
          call MUSHED (CALLER, 3, KILROY)
          write (LUEO,101) LAB,LABO,LABO,LAB
  101     format(' ','AKALON: Values of ',A,' were replaced by ',
     $               '(1 - ',A,').'/
     $           ' ','To prevent this automatic replacement, remove ',
     $               'the obsolete ',A,' input values and'/
     $           ' ','provide appropriate values of ',A,' instead.')
        end if
C
      end if
C     !END
      call BYE ('AKALON')
C
      return
      end
