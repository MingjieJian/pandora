      subroutine MOREN
     $(NO,N,NL,KAMB,HESIM,HND,HEND,RABD,XNTI,RRNT,XNNT,XNK,XND)
C
C     Rudolf Loeser, 1998 May 26
C---- Prints, for RABAM.
C     !DASH
      save
C     !DASH
      real*8 HEND, HND, RABD, RRNT, XND, XNK, XNNT, XNTI
      integer KAMB, N, NL, NO
      logical HESIM, PRNTZ
      character TIT*4
C     !DASH
      external LINER, PRIVET, ROMA, HI, BYE
C
C               XND(N,NL), HEND(N), RABD(N), XNTI(N), RRNT(N), HND(N),
      dimension XND(*),    HEND(*), RABD(*), XNTI(*), RRNT(*), HND(*),
C
C               XNK(N), XNNT(N)
     $          XNK(*), XNNT(*)
C
      data PRNTZ /.false./
C
      call HI ('MOREN')
C     !BEG
      if(NO.gt.0) then
        call LINER    (1,NO)
        if(KAMB.eq.1) then
          TIT = 'HND '
          write (NO,100) TIT
  100     format(' ',A)
          call PRIVET (NO,HND,N)
        else if((KAMB.eq.2).or.(KAMB.eq.3)) then
          TIT = 'HEND'
          write (NO,100) TIT
          call PRIVET (NO,HEND,N)
        end if
C
        call LINER    (1,NO)
        write (NO,100) 'RABD'
        call PRIVET   (NO,RABD,N)
C
        call LINER    (1,NO)
        write (NO,101) TIT
  101   format(' ','NTI = prescribed total = RABD * ',A)
        call PRIVET   (NO,XNTI,N)
C
        call LINER    (1,NO)
        write (NO,102)
  102   format(' ','NNT = computed total = NK + ND(all-levels sum)')
        call PRIVET   (NO,XNNT,N)
C     !EJECT
        call LINER    (1,NO)
        write (NO,103)
  103   format(' ','RRNT = NTI / NNT')
        call PRIVET   (NO,RRNT,N)
C
        if(.not.HESIM) then
          call LINER  (1,NO)
          write (NO,104)
  104     format(' ','NK(final) = RRNT * NK')
          call PRIVET (NO,XNK,N)
C
          call LINER  (2,NO)
          write (NO,105)
  105     format(' ','ND(final) = RRNT * ND(new)')
          call ROMA   (NO,N,NL, 1,NL, XND, 'Level ',PRNTZ)
        end if
      end if
C     !END
      call BYE ('MOREN')
C
      return
      end
