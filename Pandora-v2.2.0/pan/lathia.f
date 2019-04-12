      subroutine LATHIA
     $(X,W,N,RHEAB,HEND,HND,KVLG,VBMB,VM)
C     Rudolf Loeser, 1998 Mar 31
C---- Updates HEND and VM, for THALIA.
C     !DASH
      save
C     !DASH
      real*8 HEND, HND, RHEAB, VBMB, VM, W, X
      integer KVLG, N
C     !DASH
      external  BRYMBO, ARRMUL, ANIAN, HI, BYE
C
      dimension X(*), W(*)
C
C               RHEAB(N), VBMB(N), HEND(N), HND(N), VM(N)
      dimension RHEAB(*), VBMB(*), HEND(*), HND(*), VM(*)
C
      call HI ('LATHIA')
C     !BEG
      call BRYMBO  (N,RHEAB,HEND)
      call ARRMUL  (HEND,HND,HEND,N)
C
      if(KVLG.gt.0) then
        call ANIAN (X,W,N,RHEAB,VBMB,VM)
      end if
C     !END
      call BYE ('LATHIA')
C
      return
      end
