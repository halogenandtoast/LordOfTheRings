module LordOfTheRings.Enemy.Cards.GreatWargChief (greatWargChief) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Warg))

newtype GreatWargChief = GreatWargChief EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

greatWargChief :: EnemyCard GreatWargChief
greatWargChief = enemy GreatWargChief Cards.greatWargChief

-- | Cannot have attachments.
instance HasModifiersFor GreatWargChief where
  getModifiersFor (GreatWargChief a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Great Warg Chief engages you, discard cards from the encounter deck until
-- a Warg enemy is discarded. Put that enemy into play engaged with you.
instance HasAbilities GreatWargChief where
  getAbilities (GreatWargChief a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage GreatWargChief where
  runMessage msg e@(GreatWargChief attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == attrs.id]) \pid ->
        discardEncounterCardsUntilThen attrs warg \card -> do
          removeFromEncounterDiscard card
          putEnemyIntoPlayEngaged pid card
      pure e
    _ -> GreatWargChief <$> runMessage msg attrs
   where
    warg = CardMatchAll [CardWithType EnemyType, CardWithTrait Warg]
