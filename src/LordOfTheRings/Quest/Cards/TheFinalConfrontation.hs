module LordOfTheRings.Quest.Cards.TheFinalConfrontation (theFinalConfrontation) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheFinalConfrontation = TheFinalConfrontation QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFinalConfrontation :: QuestCard TheFinalConfrontation
theFinalConfrontation = quest TheFinalConfrontation Cards.theFinalConfrontation

-- | Encounter card effects cannot be canceled.
-- The number of damage tokens on Thane Ulchor cannot exceed the number of progress tokens
-- here.
instance HasModifiersFor TheFinalConfrontation where
  getModifiersFor (TheFinalConfrontation a) = when a.flipped do
    modified a GameTarget [CannotCancelEncounterEffects]
    thanes <- select thaneUlchor
    for_ thanes \t -> modified a t [MaxDamage a.progress]

{- | When Revealed: Reveal the top card of The Power of Mordor deck. Shuffle the encounter
discard pile into the encounter deck. Thane Ulchor makes an immediate attack against each
player in turn order.
When Thane Ulchor is defeated, the players win the game.
-}
instance RunMessage TheFinalConfrontation where
  runMessage msg q@(TheFinalConfrontation attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheFinalConfrontation <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      void revealTopOfPowerOfMordorDeck
      shuffleEncounterDiscardIntoDeck AnyCard
      thanes <- select thaneUlchor
      for_ thanes \t -> eachPlayer \p -> immediateAttack p t
      pure q
    Destroy _ (EnemyTarget eid) -> do
      defeated <- selectAny (EnemyWithId eid <> thaneUlchor)
      when defeated winGame
      TheFinalConfrontation <$> runMessage msg attrs
    _ -> TheFinalConfrontation <$> runMessage msg attrs
