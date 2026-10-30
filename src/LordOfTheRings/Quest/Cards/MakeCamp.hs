module LordOfTheRings.Quest.Cards.MakeCamp (makeCamp) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype MakeCamp = MakeCamp QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
makeCamp :: QuestCard MakeCamp
makeCamp = quest MakeCamp Cards.makeCamp

-- | Characters cannot be healed.
instance HasModifiersFor MakeCamp where
  getModifiersFor (MakeCamp a) = do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [CannotBeHealed]

-- | Response: After Make Camp is defeated, each player heals up to 3 damage from a hero he
-- controls. The players gain by it, so it is taken; Make Camp has left play by the time
-- the healing resolves, so it no longer bars it.
instance RunMessage MakeCamp where
  runMessage msg (MakeCamp attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> do
        heroes <- filterM (fmap (> 0) . getDamage . toTarget) =<< select (HeroControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "makeCamp.heal"
          causedBy attrs
          targets heroes \h -> healDamage attrs h 3
      MakeCamp <$> runMessage msg attrs
    _ -> MakeCamp <$> runMessage msg attrs
