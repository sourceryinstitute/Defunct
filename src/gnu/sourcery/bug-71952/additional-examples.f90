  type particles
    real x(1)
  end type
  type(particles) outbox(1)[*]
  print *,outbox(1)[1]%x(1:1) 
  print *,outbox(1)[1]%x(:)   
  print *,outbox(1)[1]%x      
end 
