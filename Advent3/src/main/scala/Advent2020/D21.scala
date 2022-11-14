package Advent2020

import Shared.{D, Helpers}

import scala.annotation.tailrec
import scala.collection.mutable

opaque type Ingredient = String
opaque type Allergen = String
object Ingredient:
  def Set(i: String) = i.split(' ').toSet[Ingredient]
  given Ordering[Ingredient] = Ordering.String
object Allergen:
  def Set(i: String) = i.split(", ").toSet[Allergen]
  given Ordering[Allergen] = Ordering.String

case class Recipe(ingredients: Set[Ingredient], allergens: Set[Allergen])
object Recipe extends Helpers:
  def apply(ingreds: String, allergens: String) = new Recipe(Ingredient Set ingreds, Allergen Set allergens)

object D21 extends D:
  val recipes = input.split('\n').map { s =>
    val Array(x, y) = s.stripSuffix(")").split(raw" \(contains ")
    Recipe(x, y)
  }
  val possible_defns = recipes.flatMap(_.allergens.map { allergen =>
    allergen -> recipes.collect { case r if r.allergens contains allergen => r.ingredients }.reduce { _ intersect _ }
  }).to(mutable.SortedMap)

  val all_possible_allergens = possible_defns.values.flatten.toSet
  recipes.map(_.ingredients.count(i => !all_possible_allergens(i))).sum.part

  /** @return `true` if all sizes are 1 */
  def simplify_defns: Unit = possible_defns.keySet.foreach { singletonAllergen =>
    if (possible_defns(singletonAllergen).sizeIs == 1) possible_defns.mapValuesInPlace { case (a, possible) =>
      if (a != singletonAllergen) possible - possible_defns(singletonAllergen).head
      else possible
    }
  }

  while possible_defns.values.exists(_.sizeIs > 1) do simplify_defns
  possible_defns.values.flatten.mkString(",").part
