  implicit none
  type particles
    real x(1)
  end type
  type(particles) outbox(1)[*],foo
  print *,outbox(1)[1]%x
end 
