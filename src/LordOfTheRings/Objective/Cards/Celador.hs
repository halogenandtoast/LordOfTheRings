module LordOfTheRings.Objective.Cards.Celador (celador) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype Celador = Celador ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally in the staging area, until The Hidden Way gives him to the first
-- player.
celador :: ObjectiveCard Celador
celador = objective Celador Cards.celador

-- | While Celador is in the staging area, he is committed to the current quest.
instance HasModifiersFor Celador where
  getModifiersFor (Celador a) = when (a.placement == StagingArea) $ modified a a [CountsAsCommitted]

-- | Forced: After players quest unsuccessfully or a character leaves play, deal 1 damage
-- to Celador.
instance HasAbilities Celador where
  getAbilities (Celador a) =
    [ mkAbility a 1 $ forced $ QuestedUnsuccessfullyWindow After
    , mkAbility a 2 $ forced $ CharacterLeftPlayWindow After AnyCard
    ]

-- | If Celador leaves play, remove him from the game.
instance RunMessage Celador where
  runMessage msg o@(Celador attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) _ _ -> do
      dealDamage attrs attrs 1
      pure o
    Destroy _ (isTarget attrs -> True) -> do
      removeFromEncounterDiscard $ MkEncounterCard attrs.cardId (toCardCode attrs)
      pure o
    _ -> Celador <$> runMessage msg attrs
