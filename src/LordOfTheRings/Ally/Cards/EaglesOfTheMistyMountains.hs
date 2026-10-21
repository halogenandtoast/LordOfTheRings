module LordOfTheRings.Ally.Cards.EaglesOfTheMistyMountains (eaglesOfTheMistyMountains) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), PlayerCard (..))
import LordOfTheRings.Trait (Trait (Eagle))

newtype EaglesOfTheMistyMountains = EaglesOfTheMistyMountains AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

eaglesOfTheMistyMountains :: AllyCard EaglesOfTheMistyMountains
eaglesOfTheMistyMountains = ally EaglesOfTheMistyMountains Cards.eaglesOfTheMistyMountains

-- | Eagles of the Misty Mountains cannot have restricted attachments. Eagles of the Misty
-- Mountains gets +1 attack and +1 defense for each facedown attachment it has.
instance HasModifiersFor EaglesOfTheMistyMountains where
  getModifiersFor (EaglesOfTheMistyMountains a) = do
    let n = length a.allyFacedown
    when (n > 0) $ modified a a [AttackModifier n, DefenseModifier n]

-- | Response: After another Eagle character leaves play, you may attach that card facedown
-- to Eagles of the Misty Mountains.
instance HasAbilities EaglesOfTheMistyMountains where
  getAbilities (EaglesOfTheMistyMountains a) = [mkAbility a 1 $ response $ CharacterLeftPlayWindow After (CardWithTrait Eagle)]

instance RunMessage EaglesOfTheMistyMountains where
  runMessage msg s@(EaglesOfTheMistyMountains a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [c | Window _ (CharacterLeftPlay (PlayerCard c)) <- windows, c.pcId /= a.allyCardId] \c ->
        for_ c.owner \owner -> do
          -- it is taken from where it went, its owner's discard pile
          inDiscard <- any ((== c.pcId) . (.id)) . (.discard) <$> getPlayer owner
          when inDiscard do
            removeFromDiscard owner c.pcId
            attachFacedown a.id c
      pure s
    _ -> EaglesOfTheMistyMountains <$> runMessage msg a
