module LordOfTheRings.Objective.Cards.Assassination (assassination) where

import LordOfTheRings.Card (cdThreatCost, toCardDef)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Outcome (Outcome (Lost))

newtype Assassination = Assassination ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

assassination :: ObjectiveCard Assassination
assassination = objective Assassination Cards.assassination

-- | Counts as a Plot attachment with the text: "Each enemy gets +1 attack. Attached hero
-- cannot be healed. Damage from undefended attacks and A Knife in the Back must be assigned
-- to attached hero. If attached hero leaves play, the players lose the game." The
-- undefended attacks are those of the enemies engaged with his controller.
instance HasModifiersFor Assassination where
  getModifiersFor (Assassination a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      modified a host [CannotBeHealed]
      owner <- (.owner) <$> getHero hid
      enemies <- select AnyEnemy
      engaged <- select (EnemyEngagedWith $ PlayerWithId owner)
      for_ enemies \e -> modified a e $ AttackModifier 1 : [UndefendedDamageTo host | e `elem` engaged]
    _ -> pure ()

-- | When Revealed: Attach Assassination to the hero with the highest threat cost: the
-- first player picks among those tied. A Plot is added to the staging area from out of
-- play, not revealed from the encounter deck, so this resolves as it is there.
instance RunMessage Assassination where
  runMessage msg (Assassination attrs) = case msg of
    CheckWindows _ | attrs.placement == StagingArea -> do
      heroes <- traverse (\h -> (h,) . fromMaybe 0 . cdThreatCost . toCardDef <$> getHero h) =<< select AnyHero
      let highest = foldr (max . snd) 0 heroes
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "assassination.attach"
        causedBy attrs
        targets [h | (h, c) <- heroes, c == highest] $ attachObjective attrs.id
      Assassination <$> runMessage msg attrs
    CheckWindows ws | AttachedTo host <- attrs.placement, any (isLeaving host) ws -> do
      gameOver Lost
      Assassination <$> runMessage msg attrs
    _ -> Assassination <$> runMessage msg attrs
   where
    isLeaving host = \case
      Window _ (CharacterDestroyed t _) -> t == host
      _ -> False
