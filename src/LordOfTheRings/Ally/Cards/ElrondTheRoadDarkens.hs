module LordOfTheRings.Ally.Cards.ElrondTheRoadDarkens (elrondTheRoadDarkens) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype ElrondTheRoadDarkens = ElrondTheRoadDarkens AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

elrondTheRoadDarkens :: AllyCard ElrondTheRoadDarkens
elrondTheRoadDarkens = ally ElrondTheRoadDarkens Cards.elrondTheRoadDarkens

-- | At the end of the round, discard Elrond from play.
instance HasModifiersFor ElrondTheRoadDarkens where
  getModifiersFor (ElrondTheRoadDarkens a) = modifySelf a [DiscardAtEndOfRound]

-- | Response: After Elrond enters play, choose one: heal all damage on a hero, discard a
-- Condition attachment, or each player draws 1 card.
instance HasAbilities ElrondTheRoadDarkens where
  getAbilities (ElrondTheRoadDarkens a) = [mkAbility a 1 $ response $ entersPlay a]

instance RunMessage ElrondTheRoadDarkens where
  runMessage msg s@(ElrondTheRoadDarkens a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      conditions <- getConditionAttachments
      chooseOneM pid do
        questionLabeled "elrondTheRoadDarkens.chooseOne"
        causedBy a
        labeled "elrondTheRoadDarkens.heal" do
          chooseOrRunOneM pid do
            questionLabeled "elrondTheRoadDarkens.hero"
            causedBy a
            targets heroes \h -> healAllDamage a h
        when (notNull conditions) $ labeled "elrondTheRoadDarkens.discard" do
          chooseOrRunOneM pid do
            questionLabeled "elrondTheRoadDarkens.condition"
            causedBy a
            targets conditions discard
        labeled "elrondTheRoadDarkens.draw" $ eachPlayer \p -> drawCards a p 1
      pure s
    _ -> ElrondTheRoadDarkens <$> runMessage msg a
