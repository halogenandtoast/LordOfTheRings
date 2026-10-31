module LordOfTheRings.Objective.Cards.ThaurdirsLegacy (thaurdirsLegacy, thaurdirsBurden, moveToHeroWithMost) where

import LordOfTheRings.Hero.Types (Hero)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Sorcery))

newtype ThaurdirsLegacy = ThaurdirsLegacy ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden of the Angmar Awakened campaign, from Deadmen's Dike. Setup: Attach to
Thaurdir (even if he is out of play), which the scenario does as he enters play.
Forced: After a Sorcery treachery is revealed, move Thaurdir's Legacy to the hero with the
most resources. (Counts as a Sorcery attachment with the text: "Attached hero gets -2
willpower and is treated as if its printed text box were blank.")
-}
thaurdirsLegacy :: ObjectiveCard ThaurdirsLegacy
thaurdirsLegacy = objective ThaurdirsLegacy Cards.thaurdirsLegacy

instance HasModifiersFor ThaurdirsLegacy where
  getModifiersFor (ThaurdirsLegacy a) = case a.placement of
    AttachedTo host@(HeroTarget _) -> modified a host [WillpowerModifier (-2), Blank]
    _ -> pure ()

instance HasAbilities ThaurdirsLegacy where
  getAbilities (ThaurdirsLegacy a) = thaurdirsBurden a

instance RunMessage ThaurdirsLegacy where
  runMessage msg s@(ThaurdirsLegacy a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      moveToHeroWithMost a (pure . (.resources))
      pure s
    _ -> ThaurdirsLegacy <$> runMessage msg a

-- | Its Forced effect, while attached to Thaurdir or a hero.
thaurdirsBurden :: ObjectiveAttrs -> [Ability]
thaurdirsBurden a = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Sorcery | AttachedTo _ <- [a.placement]]

-- | The burden moves to the hero with the most of something, the first player choosing
-- among those tied.
moveToHeroWithMost :: ReverseQueue m => ObjectiveAttrs -> (Hero -> m Int) -> m ()
moveToHeroWithMost a countOf = do
  heroes <- traverse getHero =<< select AnyHero
  counted <- traverse (\h -> (h.id,) <$> countOf h) heroes
  let most = maximumMay (map snd counted)
  lead <- getLead
  chooseOrRunOneM lead do
    questionLabeled "thaurdirsBurden.move"
    causedBy a
    targets [h | (h, n) <- counted, Just n == most] (attachObjective a.id)
