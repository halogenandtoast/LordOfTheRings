module LordOfTheRings.Enemy.Cards.EasterlingCaptain (easterlingCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Easterling))

newtype EasterlingCaptain = EasterlingCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingCaptain :: EnemyCard EasterlingCaptain
easterlingCaptain = enemy EasterlingCaptain Cards.easterlingCaptain

-- | Forced: When Easterling Captain enters play, discard cards from the encounter deck until
-- an Easterling treachery is discarded. Attach that treachery to Easterling Captain.
-- Forced: When Easterling Captain would take any amount of damage, discard an Easterling
-- attachment from him. Then, cancel the damage. With none on him there is nothing to
-- discard, and the damage, which only follows from it, is taken.
instance HasAbilities EasterlingCaptain where
  getAbilities (EasterlingCaptain a) =
    [ mkAbility a 1 $ forced $ MatchingEnemyEntersPlayWindow After (EnemyWithId a.id)
    , restricted a 2 (EnemyExists $ EnemyWithId a.id <> EnemyWithAttached Easterling) $ forced $ DealtDamageWindow When (toTarget a)
    ]

instance RunMessage EasterlingCaptain where
  runMessage msg e@(EasterlingCaptain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardEncounterCardsUntil attrs easterlingTreachery
      pure e
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (find (cardMatches easterlingTreachery) cards) \c -> attachFromEncounterDiscard c attrs
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      attached <- attachedWithTrait Easterling (toTarget attrs)
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "easterlingCaptain.discard"
        causedBy attrs
        targets attached discard
      cancelDamage attrs
      pure e
    _ -> EasterlingCaptain <$> runMessage msg attrs
