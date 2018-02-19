module CKron where

-- Custom Compose instance for functors.

newtype CKron f g a= CKron (f (g a))