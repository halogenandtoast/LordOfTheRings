module LordOfTheRings.Treachery.Cards.AKnifeInTheBack (aKnifeInTheBack) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Helpers.Character (getAttack)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AKnifeInTheBack = AKnifeInTheBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aKnifeInTheBack :: TreacheryCard AKnifeInTheBack
aKnifeInTheBack = treachery AKnifeInTheBack Cards.aKnifeInTheBack

-- | When Revealed: Randomly select 1 ally controlled by the first player, if able. The
-- first player deals X damage to one of his heroes, where X is that ally's attack. Then
-- discard that ally. If no ally is discarded by this effect, A Knife in the Back gains
-- surge.
instance RunMessage AKnifeInTheBack where
  runMessage msg t@(AKnifeInTheBack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      allies <- select (AllyControlledBy $ PlayerWithId lead)
      case nonEmpty allies of
        Nothing -> revealEncounterCard
        Just _ -> do
          ally <- uniform allies
          x <- getAttack (toTarget ally)
          heroes' <- select (HeroControlledBy $ PlayerWithId lead)
          -- the hero with Assassination attached takes it (The Steward's Fear Nightmare)
          marked <-
            getsGame \g -> [h | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.assassination, AttachedTo (HeroTarget h) <- [o.placement]]
          let heroes = case filter (`elem` marked) heroes' of
                [] -> heroes'
                hs -> hs
          when (x > 0) $ chooseOrRunOneM lead do
            questionLabeledWith "aKnifeInTheBack.damage" [intVar "n" x]
            causedBy attrs
            aimedAt [ally]
            targets heroes \h -> dealDamage attrs h x
          discard ally
      pure t
    _ -> AKnifeInTheBack <$> runMessage msg attrs
