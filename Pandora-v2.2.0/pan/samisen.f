      subroutine SAMISEN
     $(NO,DORS,IB,IE,N,JLEV,M,XNU,XNUC,RNU,RCP,TR,YW,FA,FB,DETL,
     $ RALL,KSHL)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Prints tables of size M pertaining to level JLEV, for HULA.
C     !DASH
      save
C     !DASH
      real*8 FA, FB, RCP, RNU, TR, XLM, XNU, XNUC, YW
      integer I, IB, IE, J, JLEV, M, N, NO
      logical DETL, DORS, KSHL, RALL
      character TFA*4, TFB*4, TJ*4, TTR*4
C     !DASH
      external LINER, NOMAD, HI, BYE
C
C               TR(N,MRX+1), YW(N,MRX+1), FA(N,MRX+1), FB(N,MRX+1),
      dimension TR(N,*),     YW(N,*),     FA(N,*),     FB(N,*),
C
C               XNU(NSL), RNU(MRX+1), RCP(MRX+1), XNUC(NSL)
     $          XNU(*),   RNU(*),     RCP(*),     XNUC(*)
C
      data TTR,TJ,TFA,TFB /'TR', 'J', 'FA', 'FB'/
C
      call HI ('SAMISEN')
C     !BEG
      call LINER     (2, NO)
      if(DORS.and.RALL) then
        write (NO,101) (I,I=IB,IE)
  101   format(' ',6X,'Lambda',5X,'RNU',5X,'RCP',4X,'Depth',I10,7I11)
        call LINER   (1, NO)
C
        do 104 J = 1,M
          call NOMAD (RNU(J), XNU, XNUC, JLEV, XLM)
          write (NO,102) XLM,RNU(J),RCP(J),TTR,(TR(I,J),I=IB,IE)
          write (NO,103)                   TJ, (YW(I,J),I=IB,IE)
C
  102     format(' ',1X,1PE12.5,0P2F8.3,3X,A4,8F11.0)
  103     format(' ',32X,A4,1P8E11.3)
C
          if(DETL.and.(.not.KSHL).and.(M.gt.1)) then
            write (NO,103)                 TFA,(FA(I,J),I=IB,IE)
            write (NO,103)                 TFB,(FB(I,J),I=IB,IE)
          end if
C
  104   continue
      else
        write (NO,105)                               (I,I=IB,IE)
  105   format(' ',32X,'Depth',I10,7I11)
        call LINER   (1, NO)
      end if
C     !END
      call BYE ('SAMISEN')
C
      return
      end
