module LordOfTheRings.Treachery.Cards.FierceFolk (fierceFolk) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (removeTimeCounterFromActiveLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FierceFolk = FierceFolk TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 3, as its card def says.
fierceFolk :: TreacheryCard FierceFolk
fierceFolk = treachery FierceFolk Cards.fierceFolk

-- | When Revealed: Each player draws 3 cards. Until the end of the round, players cannot
-- play events.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, remove 1
-- time counter from the active location. Once the attack is over the shadow reads whether
-- any character it could have destroyed, a defender or, undefended, one of the attacked
-- player's, has left play.
-- ponytail: a character the attack could destroy that left play during it some other way
-- counts as destroyed, as Morgul Vale's shadow reads it.
instance RunMessage FierceFolk where
  runMessage msg t@(FierceFolk attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> drawCards pid 3
      roundModifier attrs GameTarget (CannotPlay $ CardWithType EventType)
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      exposed <- if null defenders then charactersControlledBy pid else pure defenders
      afterAttack eid [DoStep 1 (ResolveShadowEffect cid eid pid exposed)]
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ exposed) | cid == attrs.cardId -> do
      remaining <- selectCharacters AnyHero AnyAlly
      when (any (`notElem` remaining) exposed) $ removeTimeCounterFromActiveLocation attrs
      pure t
    _ -> FierceFolk <$> runMessage msg attrs
