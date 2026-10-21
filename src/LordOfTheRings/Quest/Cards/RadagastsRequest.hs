module LordOfTheRings.Quest.Cards.RadagastsRequest (radagastsRequest) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RadagastsRequest = RadagastsRequest QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

radagastsRequest :: QuestCard RadagastsRequest
radagastsRequest = quest RadagastsRequest Cards.radagastsRequest

-- | Response: After the quest phase begins, the first player may place X damage tokens on
-- Wilyador to look at the top 3 cards of the encounter deck. Reveal and add 1 of these
-- cards to the staging area, and discard the other 2. X is the number of players in the
-- game.
-- Forced: After a card effect heals Wilyador, remove that card from the game.
--
-- ponytail: nothing in these packs heals Wilyador before the last stage but Radagast,
-- whose removal is not kept.
instance HasAbilities RadagastsRequest where
  getAbilities (RadagastsRequest a) =
    [restricted a 1 (exists $ AllyWithCardCode $ toCardCode Objectives.wilyador) $ response $ PhaseBeginsWindow After QuestPhase]

instance RunMessage RadagastsRequest where
  runMessage msg q@(RadagastsRequest attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      wilyador <- select (AllyWithCardCode $ toCardCode Objectives.wilyador)
      for_ wilyador \w -> do
        placeTokens attrs w Damage players
        checkDefeated w
      revealOneOfTopEncounterCards pid attrs 3
      pure q
    _ -> RadagastsRequest <$> runMessage msg attrs
