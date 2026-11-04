module LordOfTheRings.Location.Cards.WoodmenVillage (woodmenVillage) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard (..), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives

newtype WoodmenVillage = WoodmenVillage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

woodmenVillage :: LocationCard WoodmenVillage
woodmenVillage = location WoodmenVillage Cards.woodmenVillage

-- | Immune to player card effects.
instance HasModifiersFor WoodmenVillage where
  getModifiersFor (WoodmenVillage a) = modified a a [ImmuneToPlayerCardEffects]

-- | Forced: When Woodmen Village is explored, add the top card of the Evil Creatures deck
-- to the staging area. Then, flip over Woodmen Village and attach Haldan to the just-added
-- enemy as a guarded objective.
instance HasAbilities WoodmenVillage where
  getAbilities (WoodmenVillage a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

{- | Haldan is the card's other side: Woodmen Village turns over, out of the locations and
so not to the discard pile, and Haldan, under its card id, is the enemy's captive. With no
card to add, nothing follows the "then" (FAQ 1.15).
-}
instance RunMessage WoodmenVillage where
  runMessage msg l@(WoodmenVillage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      addFromEvilCreaturesDeck a
      pure l
    AddedFromEvilCreaturesDeck (isSource a -> True) enemy -> do
      push $ Flip (toTarget a)
      guardCardWith (EncounterCard $ MkEncounterCard a.cardId (toCardCode Objectives.haldan)) enemy
      pure l
    _ -> WoodmenVillage <$> runMessage msg a
