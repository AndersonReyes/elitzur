package com.spotify.elitzur.cats

trait FunctionalValidator[T] extends (T => List[ValidatorWithCats.ValidationResult])
